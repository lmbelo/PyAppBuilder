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
    [JSONName('apt_location')]
    FAAptLocation: string;
    [JSONName('sdk_api_location')]
    FSdkApiLocation: string;
    [JSONName('zip_align_location')]
    FZipAlign: string;
    [JSONName('key_tool_location')]
    FKeyToolLocation: string;
  public
    property SdkBasePath: string read FSdkBasePath write FSdkBasePath;
    property JarSignerLocation: string read FApkSignerLocation write FApkSignerLocation;
    property AdbLocation: string read FAdbLocation write FAdbLocation;
    property AAptLocation: string read FAAptLocation write FAAptLocation;
    property SdkApiLocation: string read FSdkApiLocation write FSdkApiLocation;
    property ZipAlignLocation: string read FZipAlign write FZipAlign;
    property KeyToolLocation: string read FKeyToolLocation write FKeyToolLocation;
  end;

implementation

end.
