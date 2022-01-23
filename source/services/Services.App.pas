unit Services.App;

interface

uses
  Services, Architecture, PythonVersion, Model.Project;

type
  TAppService = class(TInterfacedObject, IAppServices)
  private
    function GetPreBuiltFolder(const AArchitecture: TArchitecture): string;
    function GetPythonZipFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string;
    function GetAppPythonFolder(): string;
  public
    procedure CopyAppFiles(const AModel: TProjectModel);
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TPreBuiltCopyService }

function TAppService.GetAppPythonFolder: string;
begin
  Result := TPath.Combine('assets', 'internal');
end;

function TAppService.GetPreBuiltFolder(
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'pre-built');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;
  Result := TPath.Combine(Result, 'PyApp');
end;

function TAppService.GetPythonZipFile(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;

  case APythonVersion of
    cp38: Result := TPath.Combine(Result, 'python3.8');
    cp39: Result := TPath.Combine(Result, 'python3.9');
    cp310: Result := TPath.Combine(Result, 'python3.10');
  end;

  Result := TPath.Combine(Result, 'build.zip');
end;

{ TPreBuiltCopyService }

procedure TAppService.CopyAppFiles(const AModel: TProjectModel);
begin
  var LAppPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'apps');
  LAppPath := TPath.Combine(LAppPath, AModel.ApplicationName);

  if TDirectory.Exists(LAppPath) then
    TDirectory.Delete(LAppPath, true);

  TDirectory.CreateDirectory(LAppPath);

  var LPreBuiltFolder := GetPreBuiltFolder(AModel.Architecture);
  if not TDirectory.Exists(LPreBuiltFolder) then
    raise Exception.CreateFmt('Pre-built folder not found at: %s', [LPreBuiltFolder]);

  var LPythonZipFile := GetPythonZipFile(AModel.PythonVersion, AModel.Architecture);
  if not TFile.Exists(LPythonZipFile) then
    raise Exception.CreateFmt('Python zip file not found at: %s', [LPythonZipFile]);

  //Copy the app image to the target app path
  TDirectory.Copy(LPreBuiltFolder, LAppPath);

  //Copy python zip to the target app python's path
  var LAppPythonPath := TPath.Combine(LAppPath, GetAppPythonFolder());
  //Create the /assets/internal/ folder
  if not TDirectory.Exists(LAppPythonPath) then
    TDirectory.CreateDirectory(LAppPythonPath);

  LAppPythonPath := TPath.Combine(LAppPythonPath, ExtractFileName(LPythonZipFile));

  if TFile.Exists(LAppPythonPath) then
    TFile.Delete(LAppPythonPath);

  TFile.Copy(LPythonZipFile, LAppPythonPath);
end;

end.
