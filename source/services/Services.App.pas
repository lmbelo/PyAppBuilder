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
    function GetPythonInterpreterFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string;
    function GetAppPythonFolder(): string;
    function GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string;
    function GetApkPath(const AAppName: string): string;
    function GetManifestPath(const AAppName: string): string;
    function GetAppPath(const AAppName: string): string;
  public
    procedure CopyAppFiles(const AModel: TProjectModel);
    procedure UpdateManifest(const AModel: TProjectModel);
    procedure BuildApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel);
    procedure InstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel);    
  end;

implementation

uses
  System.IOUtils, System.SysUtils, Services.Factory;

const
  APPS_FOLDER = 'apps';
  APP_IMAGE_NAME = 'PyApp';
  APP_IMAGE_APK_NAME = 'PyApp.apk';

{ TPreBuiltCopyService }

function TAppService.GetApkPath(const AAppName: string): string;
begin
  Result := TPath.Combine(GetAppPath(AAppName), 'bin');
  Result := TPath.Combine(Result, ChangeFileExt(AAppName, '.apk'));
end;

function TAppService.GetAppPath(const AAppName: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), APPS_FOLDER);
  Result := TPath.Combine(Result, AAppName);
end;

function TAppService.GetAppPythonFolder: string;
begin
  Result := TPath.Combine('assets', 'internal');
end;

function TAppService.GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine('library', 'lib');
  case AArchitecture of
    arm: TPath.Combine(Result, 'armeabi-v7a');
    aarch64: TPath.Combine(Result, 'arm64-v8a');
  end;
end;

function TAppService.GetManifestPath(const AAppName: string): string;
begin
  Result := TPath.Combine(GetAppPath(AAppName), 'AndroidManifest.xml');
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
  Result := TPath.Combine(Result, APP_IMAGE_NAME);
end;

function TAppService.GetPythonInterpreterFile(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;

  case APythonVersion of
    cp38: Result := TPath.Combine(TPath.Combine(Result, 'python3.8'), 'libpython3.8.so');
    cp39: Result := TPath.Combine(TPath.Combine(Result, 'python3.9'), 'libpython3.9.so');
    cp310: Result := TPath.Combine(TPath.Combine(Result, 'python3.10'), 'libpython3.10.so');
  end;
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

procedure TAppService.UpdateManifest(const AModel: TProjectModel);
begin
  var LManifestPath := GetManifestPath(AModel.ApplicationName);
  var LText := TFile.ReadAllText(LManifestPath, TEncoding.UTF8);
  LText := LText
    .Replace('package="com.embarcadero.PyApp"', Format('package="%s"', [AModel.PackageName]))
    .Replace('android:versionCode="1"', Format('android:versionCode="%s"', [AModel.VersionCode.ToString()]))
    .Replace('android:versionName="1.0.0"', Format('android:versionName="%s"', [AModel.VersionName]))
    .Replace('android:label="PyApp"', Format('android:label="%s"', [AModel.ApplicationName]));

  TFile.WriteAllText(LManifestPath, LText);
end;

{ TPreBuiltCopyService }

procedure TAppService.BuildApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel);
begin
  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    LService.BuildApk(GetAppPath(AProjectModel.ApplicationName), 
      AProjectModel.ApplicationName, AEnvironmentModel, LStrings);
  finally
    LStrings.Free();
  end;
end;

procedure TAppService.CopyAppFiles(const AModel: TProjectModel);
begin
  var LAppPath := GetAppPath(AModel.ApplicationName);

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

   //Copy the python zip to the app assets/internal/
  TFile.Copy(LPythonZipFile, LAppPythonPath);

  //Get the python interpreter shared lib place
  var LPythonInterpreterFile := GetPythonInterpreterFile(AModel.PythonVersion, AModel.Architecture);

  //Copy the python interpreter to the app lib
  var LAppPythonInterpreterPath := TPath.Combine(LAppPath, GetAppPythonInterpreterFolder(AModel.Architecture));

  //Create the /library/lib/ folder
  if not TDirectory.Exists(LAppPythonInterpreterPath) then
    TDirectory.CreateDirectory(LAppPythonInterpreterPath);

  //Copy the python interpreter to the app library/lib/{arch}
  TFile.Copy(LPythonInterpreterFile, LAppPythonInterpreterPath);
end;

end.
