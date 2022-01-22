unit Services.ADB;

interface

uses
  Services.IDE, System.Classes, System.SysUtils;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    procedure ExecCmd(const CmdLine: string; CmdResult: TStrings);
  private
    function GetAdbPath(): string;
    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const ADevice: string): string;
  public
    procedure ListDevices(const AStrings: TStrings);
  end;

implementation

uses
  Storage.Default,
  Model.Environment,
  {$IFDEF MSWINDOWS}
  Services.ADB.Win;
  {$ELSE}
  Services.ADB.Posix;
  {$ENDIF}

{ TADBService }

procedure TADBService.EnumDevices(const ADeviceList: TStrings;
  const AProc: TProc<string>);
begin
  if not Assigned(AProc) then
    Exit;

  if ADeviceList.Count > 1 then begin
    for var I := 1 to ADeviceList.Count -1 do begin
      if ADeviceList[I].Trim().IsEmpty() then
        Exit;

      var LPos := Pos(#9, ADeviceList[I]);
      if LPos = -1 then
        Exit;

      var LDevice := Copy(ADeviceList[I], 1, LPos - 1);
      var LValue: extended;
      if TryStrToFloat(LDevice, LValue) then
        AProc(LDevice);
    end;
  end;
end;

procedure TADBService.ExecCmd(const CmdLine: string; CmdResult: TStrings);
begin
  var LLog: ILogServices := nil;
  if Supports(GlobalServices, ILogServices, LLog) then
    LLog.Log('ExecCmd: ' + CmdLine);

  ExecCmdine(CmdLine, CmdResult);

  if Assigned(LLog) then
    LLog.Log(CmdResult.Text);
end;

function TADBService.FindDeviceVendorModel(const ADevice: string): string;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(GetAdbPath() + Format(' -s %s shell getprop ro.product.model', [ADevice]), LStrings);
    Result := LStrings.Text
      .Replace(#13#10, String.Empty);
  finally
    LStrings.Free();
  end;
end;

function TADBService.GetAdbPath: string;
begin
  var LStorage := TDefaultStorage<TEnvironmentModel>.Make();
  var LModel: TEnvironmentModel := nil;
  if LStorage.LoadModel(LModel) then
    Result := LModel.AdbLocation
  else
    Result := string.Empty;
end;

procedure TADBService.ListDevices(const AStrings: TStrings);
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(GetAdbPath() + ' devices', LStrings);
    EnumDevices(LStrings, procedure(ADevice: string) begin
      AStrings.Add(FindDeviceVendorModel(ADevice));
    end);
  finally
    LStrings.Free();
  end;
end;

end.