unit uAocGrid;

interface

uses
  System.Generics.Collections,
  classes, System.SysUtils,
  uAocUtils;

type
  TCharToValueConverter<TValue> = function(const aChar: Char): TValue of object;
  TValueToCharConverter<TValue> = function(const aValue: TValue): Char of object;

  TAocGrid<TValue> = class
  private
    FMaxX, FMaxY: integer;
    FValueConverter: TValueToCharConverter<TValue>;

    type
      TAocGridEnumerator = class(TEnumerator<TPair<TPosition,TValue>>)
      private
        FGrid: TAocGrid<TValue>;
        CurrentX, CurrentY: integer;
        function GetCurrent: TPair<TPosition,TValue>; inline;
      protected
        function DoGetCurrent: TPair<TPosition,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const aGrid: TAocGrid<TValue>);
        function MoveNext: Boolean; inline;
        property Current: TPair<TPosition,TValue> read GetCurrent;
      end;

  protected
    procedure CreateDataHolder; virtual; abstract;
  public
    constructor Create(aStrings: TStrings; aCharToValueConverter: TCharToValueConverter<TValue>; aValueToCharConverter: TValueToCharConverter<TValue>); reintroduce; overload;
    constructor Create(aMaxX, aMaxY: integer; aValueToCharConverter: TValueToCharConverter<TValue> = nil) overload;
    destructor Destroy; override;

    procedure PrintToDebug;
    procedure SetData(aPosition: TPosition; aValue: TValue); overload; virtual; abstract;
    procedure SetData(aX, aY: integer; aValue: TValue); overload;
    function TryGetValue(aX, aY: integer; out aValue: TValue): boolean; overload; virtual;
    function TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean; overload; virtual; abstract;
    function GetValue(aX, aY: integer): TValue; overload;
    function GetValue(aPosition: TPosition): TValue; overload; virtual; abstract;
    function GetEnumerator: TAocGridEnumerator; reintroduce;

    property MaxX: integer read FMaxX;
    property MaxY: integer read FMaxY;
  end;

  TAocStaticGrid<TValue> = class(TAocGrid<TValue>)
  private
    FData: Array of TValue;
    function KeyIndex(const aX, aY: integer): integer; inline;
  protected
    procedure CreateDataHolder; override;
  public
    destructor Destroy; override;

    procedure SetData(aPosition: TPosition; aValue: TValue); override;
    function TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean; overload; override;
    function TryGetValue(aX, aY: integer; out aValue: TValue): boolean; overload; override;
    function GetValue(aX, aY: integer): TValue; overload; inline;
    function GetValue(aPosition: TPosition): TValue; overload; override;
  end;

  TAocDynamicGrid<TValue> = class(TAocGrid<TValue>)
  private
    FData: TDictionary<int64,TValue>;
  protected
    procedure CreateDataHolder; override;
  public
    destructor Destroy; override;

    procedure SetData(aPosition: TPosition; aValue: TValue); override;
    function TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean; overload; override;
    function GetValue(aPosition: TPosition): TValue; overload; override;
  end;

  TAocGridHelper = class
  private
    class function CharToInt(const aChar: Char): Integer;
    class function IntToChar(const aInt: integer): char;
  public
    class function CharToChar(const aChar: Char): Char;
    class function BoolToChar(const aBool: Boolean): Char;

    class function CreateCharGrid(aStrings: TStrings; asDynamicGrid: boolean = false): TAocGrid<Char>;
    class function CreateIntegerGrid(aStrings: TStrings; asDynamicGrid: boolean = false): TAocGrid<Integer>;
  end;

implementation


constructor TAocGrid<TValue>.create(aStrings: TStrings; aCharToValueConverter: TCharToValueConverter<TValue>; aValueToCharConverter: TValueToCharConverter<TValue>);
var
  tmpX, tmpY: Integer;
begin
  Create(Length(aStrings[0]), aStrings.Count, aValueToCharConverter);

  for tmpY := 0 to MaxY-1 do
    for tmpX := 0 to MaxX-1 do
      SetData(TPosition.Create(tmpX, tmpY), aCharToValueConverter(aStrings[tmpY][tmpX+1]));
end;

constructor TAocGrid<TValue>.Create(aMaxX, aMaxY: integer; aValueToCharConverter: TValueToCharConverter<TValue> = nil);
begin
  FMaxX := aMaxX;
  FMaxY := aMaxY;
  FValueConverter := aValueToCharConverter;

  CreateDataHolder;
end;

destructor TAocGrid<TValue>.Destroy;
begin
  inherited;
end;

procedure TAocGrid<TValue>.PrintToDebug;
var
  x, y: integer;
  s: string;
begin
  Writeln('');
  for y := 0 to MaxY -1 do
  begin
    s := '';
    for x := 0 to MaxX -1 do
      s := s + FValueConverter(GetValue(TPosition.Create(x, y)));
    Writeln(s);
  end;
end;

procedure TAocGrid<TValue>.SetData(aX, aY: integer; aValue: TValue);
begin
  SetData(TPosition.Create(aX, aY), aValue);
end;

function TAocGrid<TValue>.TryGetValue(aX, aY: integer; out aValue: TValue): boolean;
begin
  Result := TryGetValue(TPosition.Create(aX, aY), aValue);
end;

function TAocGrid<TValue>.GetValue(aX, aY: integer): TValue;
begin
  Result := GetValue(TPosition.Create(aX, aY));
end;

function TAocGrid<TValue>.GetEnumerator: TAocGridEnumerator;
begin
  Result := TAocGridEnumerator.Create(Self);
end;

{ TAocGrid<TValue>.TAocGridEnumerator }

constructor TAocGrid<TValue>.TAocGridEnumerator.Create(const aGrid: TAocGrid<TValue>);
begin
  FGrid := aGrid;
  CurrentX := -1;
end;

function TAocGrid<TValue>.TAocGridEnumerator.DoGetCurrent: TPair<TPosition, TValue>;
begin
  Result := GetCurrent;
end;

function TAocGrid<TValue>.TAocGridEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TAocGrid<TValue>.TAocGridEnumerator.GetCurrent: TPair<TPosition, TValue>;
var
  Key: TPosition;
begin
  Key := TPosition.Create(CurrentX, CurrentY);
  Result.Key := Key;
  Result.Value := FGrid.GetValue(Key);
end;

function TAocGrid<TValue>.TAocGridEnumerator.MoveNext: Boolean;
begin
  inc(CurrentX);
  if not InRange(CurrentX, 0, FGrid.MaxX-1) then
  begin
    CurrentX := 0;
    Inc(CurrentY);
  end;
  Result := InRange(CurrentY, 0, FGrid.MaxY-1)
end;

{ TAocGridHelper }

class function TAocGridHelper.CreateCharGrid(aStrings: TStrings; asDynamicGrid: boolean = false): TAocGrid<Char>;
begin
  if asDynamicGrid then
  begin
    Result := TAocDynamicGrid<Char>.create(aStrings, CharToChar, CharToChar);
    exit;
  end;

  Result := TAocStaticGrid<Char>.create(aStrings, CharToChar, CharToChar);
end;

class function TAocGridHelper.CreateIntegerGrid(aStrings: TStrings; asDynamicGrid: boolean = false): TAocGrid<Integer>;
begin
  if asDynamicGrid then
  begin
    Result := TAocDynamicGrid<Integer>.create(aStrings, CharToInt, IntToChar);
    exit;
  end;

  Result := TAocStaticGrid<Integer>.create(aStrings, CharToInt, IntToChar);
end;

class function TAocGridHelper.BoolToChar(const aBool: Boolean): Char;
begin
  Result := '.';
  if aBool then
    Result := '#';
end;

class function TAocGridHelper.CharToChar(const aChar: Char): Char;
begin
  Result := aChar;
end;

class function TAocGridHelper.CharToInt(const aChar: Char): Integer;
begin
  Result := StrToInt(aChar);
end;

class function TAocGridHelper.IntToChar(const aInt: integer): char;
begin
  Assert(aInt < 10);
  Result := aInt.ToString[1];
end;

{ TAocStaticGrid<TValue> }

procedure TAocStaticGrid<TValue>.CreateDataHolder;
begin
  SetLength(FData, MaxX * MaxY);
end;

function TAocStaticGrid<TValue>.KeyIndex(const aX, aY: integer): integer;
begin
  Result := aX * MaxY + aY;
end;

destructor TAocStaticGrid<TValue>.Destroy;
begin
  inherited;
end;

function TAocStaticGrid<TValue>.GetValue(aX, aY: integer): TValue;
begin
  Result := FData[KeyIndex(aX, aY)];
end;

function TAocStaticGrid<TValue>.GetValue(aPosition: TPosition): TValue;
begin
  Result := GetValue(aPosition.x, aPosition.y);
end;

procedure TAocStaticGrid<TValue>.SetData(aPosition: TPosition; aValue: TValue);
begin
  FData[KeyIndex(aPosition.x, aPosition.Y)] := aValue;
end;

function TAocStaticGrid<TValue>.TryGetValue(aX, aY: integer; out aValue: TValue): boolean;
begin
  Result := InRange(aX, 0, MaxX-1) and InRange(aY, 0, MaxY-1);
  if Result then
    aValue := GetValue(aX, aY)
  else
    aValue := Default(TValue);
end;

function TAocStaticGrid<TValue>.TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean;
begin
  Result := TryGetValue(aPosition.x, aPosition.y, aValue)
end;

{ TAocDynamicGrid<TValue> }

procedure TAocDynamicGrid<TValue>.CreateDataHolder;
begin
  FData := TDictionary<Int64,TValue>.Create(MaxX * MaxY);
end;

destructor TAocDynamicGrid<TValue>.Destroy;
begin
  FData.Free;
  inherited;
end;

function TAocDynamicGrid<TValue>.GetValue(aPosition: TPosition): TValue;
begin
  Result := FData[aPosition.CacheKey];
end;

procedure TAocDynamicGrid<TValue>.SetData(aPosition: TPosition; aValue: TValue);
begin
  FData.AddOrSetValue(aPosition.CacheKey, aValue);
end;

function TAocDynamicGrid<TValue>.TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean;
begin
  Result := FData.TryGetValue(aPosition.CacheKey, aValue);
end;

end.
