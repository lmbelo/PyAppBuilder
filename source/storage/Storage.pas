unit Storage;

interface

uses
  System.TypInfo;

type
  IStorage = interface
    ['{BF1A532A-66D2-4D8B-A8A2-B356A6FC7CC9}']
    procedure SaveModel(const ATypeInfo: PTypeInfo; const AModel: TObject; const AEntity: string = '');
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject; const AEntity: string = ''): boolean;
  end;

  IStorage<Model : Class> = interface(IStorage)
    ['{37366D5D-ECDA-4282-8762-CF87B7B440F7}']
    procedure SaveModel(const AModel: Model; const AEntity: string = '');
    function LoadModel(var AModel: Model; const AEntity: string = ''): boolean;
  end;

implementation

end.
