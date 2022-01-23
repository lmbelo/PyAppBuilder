unit Model.Project;

interface

uses
  REST.Json.Types, Model, Architecture, PythonVersion;

type
  [Model('project')]
  TProjectModel = class
  private
    [JSONName('application_name')]
    FApplicationName: string;
    [JSONName('package_name')]
    FPackageName: string;
    [JSONName('version_code')]
    FVersionCode: integer;
    [JSONName('version_name')]
    FVersionName: string;
    [JSONName('python_version')]
    FPythonVersion: TPythonVersion;
    [JSONName('architecture')]
    FArchitecture: TArchitecture;
  public
    property ApplicationName: string read FApplicationName write FApplicationName;
    property PackageName: string read FPackageName write FPackageName;
    property VersionCode: integer read FVersionCode write FVersionCode;
    property VersionName: string read FVersionName write FVersionName;
    property PythonVersion: TPythonVersion read FPythonVersion write FPythonVersion;
    property Architecture: TArchitecture read FArchitecture write FArchitecture;
  end;

implementation

end.
