unit Services.ADB;

interface

uses
  Services, System.Classes, System.SysUtils;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    procedure ExecCmd(const CmdLine: string; CmdResult: TStrings);
  private
    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
  public
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    procedure InstallApk(const AAdbPath: string; const AApkPath: string; const AResult: TStrings);
    procedure RunApp(const AAdbPath, APkgName: string; const AResult: TStrings);
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

function TADBService.FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + Format(' -s %s shell getprop ro.product.model', [ADevice]), LStrings);
    Result := LStrings.Text
      .Replace(#13#10, String.Empty);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.InstallApk(const AAdbPath, AApkPath: string; const AResult: TStrings);
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + ' install -r ' + AApkPath, LStrings);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.ListDevices(const AAdbPath: string; const AStrings: TStrings);
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + ' devices', LStrings);
    EnumDevices(LStrings, procedure(ADevice: string) begin
      AStrings.Add(FindDeviceVendorModel(AAdbPath, ADevice));
    end);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.RunApp(const AAdbPath, APkgName: string;
  const AResult: TStrings);
begin
  ExecCmd(AAdbPath
    + Format(' shell am start -n %s/com.embarcadero.firemonkey.FMXNativeActivity', [
    APkgName]), AResult);
end;

end.
