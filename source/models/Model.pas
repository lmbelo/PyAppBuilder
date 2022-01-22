unit Model;

interface

type
  ModelAttribute = class(TCustomAttribute)
  private
    FModelName: string;
  public
    constructor Create(const AModelName: string); overload;

    property ModelName: string read FModelName write FModelName;
  end;

implementation

{ ModelAttribute }

constructor ModelAttribute.Create(const AModelName: string);
begin
  FModelName := AModelName;
end;

end.