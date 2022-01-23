unit Model.Environment;

interface

uses
  REST.Json.Types, Model;

type
  [Model('environment')]
  TEnvironmentModel = class
  private
    [JSONName('sdk_base_path')]
    FSdkBasePath: string;
    [JSONName('apk_signer_location')]
    FApkSignerLocation: string;
    [JSONName('adb_location')]
    FAdbLocation: string;
  public
    property SdkBasePath: string read FSdkBasePath write FSdkBasePath;
    property ApkSignerLocation: string read FApkSignerLocation write FApkSignerLocation;
    property AdbLocation: string read FAdbLocation write FAdbLocation;
  end;

implementation

end.
