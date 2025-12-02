unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, RTTI,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json,
  AocLetterReader, uAOCTimer, uAocGrid,
  System.Threading, System.SyncObjs, system.Hash;

type
  IntegerArray = Array Of Integer;

  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    ResultA, ResultB: integer;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    ResultA, ResultB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay = class(TAdventOfCode)
  private
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

implementation

{$REGION 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;
var
  Distance, CurrentIndex, PrevIndex, FullRotations: integer;
  Instruction, Rotation: string;
begin
  inherited;

  ResultA := 0;
  ResultB := 0;
  CurrentIndex := 50;

  for Instruction in FInput do
  begin
    Rotation := Instruction[1];
    Distance := StrToInt(Copy(Instruction, 2, Instruction.Length-1));

    FullRotations := Distance div 100;
    inc(ResultB, FullRotations);
    Distance := Distance mod 100;

    PrevIndex := CurrentIndex;
    case IndexText(Rotation, ['L', 'R']) of
      0: Dec(CurrentIndex, Distance);
      1: Inc(CurrentIndex, Distance);
    else
      Assert(False, 'Unkown rotation ' + Rotation);
    end;

    if (PrevIndex <> 0) and (CurrentIndex <= 0) or (CurrentIndex >=100)  then
      Inc(ResultB);

    CurrentIndex := (CurrentIndex + 100) mod 100;

    if CurrentIndex = 0 then
      inc(ResultA);
  end;
end;

function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
   Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay2'}
procedure TAdventOfCodeDay2.BeforeSolve;

  function IsValidA(aId: int64): boolean;
  var
    Length, HalfLength: integer;
    Id: string;
  begin
    id := aId.ToString;
    Length := id.Length;

    if Odd(length) then
      Exit(False);

    HalfLength := Length shr 1;
    Result := copy(id, 1, HalfLength) = copy(id, HalfLength + 1, Length-1);
  end;

  function IsValidB(aId: int64): boolean;
  var
    IdLength, SubIdLength, RemainingIdLength: integer;
    SubId, RemainingId: int64;
    Valid: boolean;
  begin
    Result := False;
    IdLength := aId.ToString.Length;

    for SubIdLength := 1 to (IdLength shr 1) do
    begin
      if IdLength mod SubIdLength <> 0 then
        Continue;

      RemainingIdLength := IdLength - SubIdLength;
      SubId := aId div Base10Table[RemainingIdLength];
      RemainingId := aId - SubId * Base10Table[RemainingIdLength];

      Valid := True;
      while (RemainingIdLength > 0) and valid do
      begin
        valid := RemainingId div Base10Table[RemainingIdLength-SubIdLength] = SubId;
        RemainingIdLength := RemainingIdLength - SubIdLength;
        RemainingId := RemainingId - SubId * Base10Table[RemainingIdLength];
      end;

      if Valid then
        Exit(True);
    end;
  end;

var
  s: string;
  split, Split2: TStringDynArray;
  i,j,x: int64;
begin
  ResultA := 0;
  ResultB := 0;

  split := SplitString(FInput[0], ',');
  for s in split do
  begin
    Split2 := SplitString(s, '-');

    i:= Split2[0].ToInt64;
    j:= Split2[1].ToInt64;

    for x := i to j do
    begin
      if IsValidA(x) then
        Inc(ResultA, x);

      if IsValidB(x) then
        Inc(ResultB, x);
    end;
  end;
end;

function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}

{$REGION 'TAdventOfCodeDay'}
procedure TAdventOfCodeDay.BeforeSolve;

begin
  inherited;


end;

procedure TAdventOfCodeDay.AfterSolve;
begin
  inherited;

end;

function TAdventOfCodeDay.SolveA: Variant;
//var
//  s: string;
//  split: TStringDynArray;
begin
//  for s in FInput do
//  begin
//    split := SplitString(s, ',');
//  end;
end;

function TAdventOfCodeDay.SolveB: Variant;
begin
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1,TAdventOfCodeDay2]);

end.
