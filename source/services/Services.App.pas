unit Services.App;

interface

uses
  Services, Architecture, PythonVersion, Model.Project, Model.Environment, 
  System.Classes;

type
  TAppService = class(TInterfacedObject, IAppServices)
  private
    function GetPreBuiltFolder(const AArchitecture: TArchitecture): string;
    function GetPythonZipFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string;
    function GetAppPythonFolder(): string;
    function GetApkPath(const AAppName: string): string;
  public
    procedure CopyAppFiles(const AModel: TProjectModel);
    procedure InstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel);
  end;

implementation

uses
  System.IOUtils, System.SysUtils, Services.Factory;

const
  APPS_FOLDER = 'APPS';

{ TPreBuiltCopyService }

function TAppService.GetApkPath(const AAppName: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), APPS_FOLDER);
  Result := TPath.Combine(Result, AAppName);
  Result := TPath.Combine(Result, 'bin');
  Result := TPath.Combine(Result, 'PyApp.apk');
end;

function TAppService.GetAppPythonFolder: string;
begin
  Result := TPath.Combine('assets', 'internal');
end;

function TAppService.GetPreBuiltFolder(
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'android');
  Result := TPath.Combine(Result, 'pre-built');
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

procedure TAppService.InstallApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel);
begin
  var LApkPath := GetApkPath(AProjectModel.ApplicationName);
  if not TFile.Exists(LApkPath) then
    raise Exception.CreateFmt('Apk file %s not found at: %s', [AProjectModel.ApplicationName, LApkPath]);

  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    LService.InstallApk(AEnvironmentModel.AdbLocation, LApkPath, LStrings);
  finally
    LStrings.Free();
  end;
end;

{ TPreBuiltCopyService }

procedure TAppService.CopyAppFiles(const AModel: TProjectModel);
begin
  var LAppPath := TPath.Combine(ExtractFilePath(ParamStr(0)), APPS_FOLDER);
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
