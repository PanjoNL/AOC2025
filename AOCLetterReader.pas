unit AOCLetterReader;

interface

uses
  System.Generics.Collections, inifiles, system.Classes, System.Sysutils;

type
  TPixelReader = reference to function(const aRowNo, aColumnNo: integer): boolean;

  TBasicAOCLetterReader = class
  private
    LetterCache: TDictionary<Int64, string>;

    procedure SetupCache;
    function MapLetter(const LetterNo: integer; PixelReader: TPixelReader): Int64;
  protected
    function LetterWidth: integer; virtual; abstract;
    function LetterHeigth: Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadLetters(aCount: Integer; PixelReader: TPixelReader): string;
  end;

  TAOCLetterReader_5_6 = class(TBasicAOCLetterReader)
  protected
    function LetterWidth: integer; override;
    function LetterHeigth: Integer; override;
  end;

implementation

uses
  uAOCUtils;

{ TBasicAOCLetterReader }

constructor TBasicAOCLetterReader.Create;
begin
  LetterCache := TDictionary<Int64, string>.Create;
  SetupCache;
end;

destructor TBasicAOCLetterReader.Destroy;
begin
  LetterCache.Free;
end;

function TBasicAOCLetterReader.MapLetter(const LetterNo: integer; PixelReader: TPixelReader): Int64;
var
  Row, Column: integer;
//  s: string;
begin
  Result := 0;
//  Writeln('');
  for Row := 0 to LetterHeigth -1 do
  begin
//    s := '';
    for Column := 0 to LetterWidth-1 do
    begin
      if PixelReader(Row, LetterNo * LetterWidth + Column) then
      begin
        Result := Result + 1 shl (row * LetterWidth + Column);
//        s := s + '#'
      end
//      else
//        s := s + '.'
    end;
//    writeLn(s);
  end;
end;

function TBasicAOCLetterReader.ReadLetters(aCount: Integer; PixelReader: TPixelReader): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to aCount-1 do
    Result := Result + LetterCache[MapLetter(i, PixelReader)];
end;

procedure TBasicAOCLetterReader.SetupCache;
var
  Ini: TIniFile;
  DataRows: TStringList;
  Defenition: string;
  Row, i: Integer;
  Mapped: Int64;
begin
  Ini := TIniFile.Create(AOCUtils.GetAocIniFilePath('AOCLetterReader.ini') + PathDelim + 'AOCLetterReader.ini');
  DataRows := TStringList.Create;
  Defenition := Ini.ReadString(Self.ClassName, 'Defenition', '');
  for Row := 0 to LetterHeigth -1 do
    DataRows.Add(Ini.ReadString(Self.ClassName, 'Row_'+IntToStr(Row), ''));

  for i := 0 to Length(Defenition)-1 do
  begin
    Mapped := MapLetter(0,
      function(const aRowNo, aColumnNo: integer): boolean
      begin
        Result := DataRows[aRowNo][aColumnNo+1 + i * LetterWidth] = '#';
      end);
    LetterCache.Add(Mapped, Defenition[i+1]);
  end;

  DataRows.Free;
  Ini.Free
end;

{ TAOCLetterReader_5_6 }

function TAOCLetterReader_5_6.LetterWidth: integer;
begin
  Result := 5;
end;

function TAOCLetterReader_5_6.LetterHeigth: Integer;
begin
  Result := 6
end;


end.
