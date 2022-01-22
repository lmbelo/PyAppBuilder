unit Services.IDE;

interface

uses
  System.Classes;

type
  IServices = interface
    ['{0959ED55-8FA5-4D88-88BB-EB2738261A23}']
  end;

  ILogServices = interface
    ['{50EDF1E4-BABC-42BD-A93D-957C53ED9663}']
    procedure Log(const AString: string);
  end;

  IADBServices = interface
    ['{BAF1EE13-B459-4EBC-9E81-7C782F285F22}']
    procedure ListDevices(const AStrings: TStrings);
  end;

var
  GlobalServices: IServices;

implementation

end.
