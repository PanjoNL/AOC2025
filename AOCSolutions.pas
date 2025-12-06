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

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    FBatteryBanks: TAocGrid<integer>;
    function FindBestBatteryConfiguration(aNumberOfBatteries: integer): int64;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    FWareHouse: TAocGrid<boolean>;
    FWareHouseFloor: TAocGrid<byte>;
    FPaperRolesToRemove: TQueue<TPosition>;
    function WareHouseTileToBool(const aChar: Char): boolean;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  type Range = record
    LowBound,
    HighBound: int64;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    FAllRanges: TList<Range>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    function CalculateSum(Operation: char; Numbers: TList<int64>): int64; inline;
  protected
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

  function SumIdsA(SeenInvalidIds: TDictionary<int64, boolean>; aNumbersLeft, aNumbersUsed: integer; aCurrentId, LowBound, HighBound: int64): int64;
  var
    i, ConstructedId: int64;
  begin
    Result := 0;

    if aNumbersLeft = 0 then
    begin
      ConstructedId := aCurrentId*Base10Table[aNumbersUsed] + aCurrentId;
      if InRange(ConstructedId, LowBound, HighBound) then
      begin
        SeenInvalidIds.AddOrSetValue(ConstructedId, True);
        Result := ConstructedId;
      end;
      exit;
    end;

    for i := ifthen(aCurrentId = 0, 1, 0) to 9 do
      inc(Result, SumIdsA(SeenInvalidIds, aNumbersLeft-1, aNumbersUsed+1, aCurrentId*10+i, LowBound, HighBound));
  end;

  function SumIdsB(SeenInvalidIds: TDictionary<int64, boolean>; aTotalNumbers, aNumbersUsed: integer; aCurrentId, LowBound, HighBound: int64): int64;
  var
    i, ConstructedId: int64;
    TmpTotalNumbers: integer;
  begin
    Result := 0;

    if (aNumbersUsed*2+1) > aTotalNumbers then // -1 since where reusing the id's found in part A
      Exit;

    if aNumbersUsed > 0 then
    begin
      ConstructedId := aCurrentId;

      TmpTotalNumbers := aNumbersUsed;
      while TmpTotalNumbers < aTotalNumbers do
      begin
        Inc(TmpTotalNumbers, aNumbersUsed);
        ConstructedId := ConstructedId * Base10Table[aNumbersUsed] + aCurrentId;

        if InRange(ConstructedId, LowBound, HighBound) and not SeenInvalidIds.ContainsKey(ConstructedId) then
        begin
          SeenInvalidIds.Add(ConstructedId, true);
          inc(Result, ConstructedId);
        end;
      end;
    end;

    for i := ifthen(aCurrentId = 0, 1, 0) to 9 do
      inc(Result, SumIdsB(SeenInvalidIds, aTotalNumbers, aNumbersUsed+1, aCurrentId*10+i, LowBound, HighBound));
  end;

var
  s: string;
  split, Split2: TStringDynArray;
  LowBound,HighBound,InvalidIdsPartA: int64;
  SeenInvalidIds: TDictionary<int64, boolean>;
begin
  ResultA := 0;
  ResultB := 0;

  SeenInvalidIds := TDictionary<int64, boolean>.Create;
  try
    split := SplitString(FInput[0], ',');
    for s in split do
    begin
      Split2 := SplitString(s, '-');
      SeenInvalidIds.Clear;

      LowBound:= Split2[0].ToInt64;
      HighBound:= Split2[1].ToInt64;

      InvalidIdsPartA := SumIdsA(SeenInvalidIds, Split2[1].Length shr 1, 0, 0, LowBound, HighBound);
      Inc(ResultA, InvalidIdsPartA);
      Inc(ResultB, InvalidIdsPartA); // Reuse the found ids of part A so we kan skip one loop in the b part
      Inc(ResultB, SumIdsB(SeenInvalidIds, Split2[1].Length, 0, 0, LowBound, HighBound));
    end;
  finally
    SeenInvalidIds.Free;
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
{$REGION 'TAdventOfCodeDay3'}
procedure TAdventOfCodeDay3.BeforeSolve;
begin
  inherited;
  FBatteryBanks := TAocGridHelper.CreateIntegerGrid(FInput, False);
end;

procedure TAdventOfCodeDay3.AfterSolve;
begin
  inherited;
  FBatteryBanks.Free;
end;

function TAdventOfCodeDay3.FindBestBatteryConfiguration(aNumberOfBatteries: integer): int64;

  function FindBestBattery(aBatteryId, aStartIndex, aBatterysLeft: integer; aCurrentJolts: int64): int64;
  var
    BestBattery, TempBattery: int64;
    BestBetteryPosistion, i: integer;
  begin
    if aBatterysLeft < 0 then
      Exit(aCurrentJolts);

    BestBattery := -1;
    BestBetteryPosistion := 0;
    for i := aStartIndex to FBatteryBanks.MaxX-aBatterysLeft-1 do
    begin
      TempBattery := FBatteryBanks.GetValue(i, aBatteryId);

      if TempBattery > BestBattery then
      begin
        BestBattery := TempBattery;
        BestBetteryPosistion := i;
      end;
    end;

    Result := FindBestBattery(aBatteryId, BestBetteryPosistion+1, aBatterysLeft-1, aCurrentJolts*10+BestBattery);
  end;

var
  batteryId: integer;
begin
  Result := 0;
  for batteryId := 0 to FBatteryBanks.MaxY-1 do
    Inc(Result, FindBestBattery(batteryId, 0, aNumberOfBatteries-1, 0));
end;

function TAdventOfCodeDay3.SolveA: Variant;
begin
  Result := FindBestBatteryConfiguration(2);
end;

function TAdventOfCodeDay3.SolveB: Variant;
begin
  Result := FindBestBatteryConfiguration(12);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay4'}
procedure TAdventOfCodeDay4.BeforeSolve;
begin
  inherited;
  FWareHouse := TAocStaticGrid<boolean>.Create(FInput, WareHouseTileToBool, nil);
  FWareHouseFloor := TAocStaticGrid<byte>.Create(FWareHouse.MaxX, FWareHouse.MaxY, nil);
  FPaperRolesToRemove := TQueue<TPosition>.Create;
end;

procedure TAdventOfCodeDay4.AfterSolve;
begin
  inherited;
  FWareHouse.Free;
  FWareHouseFloor.Free;
  FPaperRolesToRemove.Free;
end;

function TAdventOfCodeDay4.WareHouseTileToBool(const aChar: Char): boolean;
begin
  Result := aChar = '@'
end;

function TAdventOfCodeDay4.SolveA: Variant;
var
  WareHouseTile: TPair<TPosition, boolean>;
  positionToCheck: TPosition;
  ContainsPaperRole, i: integer;
  IsPaperRole: boolean;
begin
  Result := 0;
  for WareHouseTile in FWareHouse do
    if WareHouseTile.Value then
    begin
      ContainsPaperRole := 0;
      for i := 0 to 7 do
      begin
        positionToCheck := WareHouseTile.Key.Clone.ApplyDirections(DirectionsAround[i], 1);
        if FWareHouse.TryGetValue(positionToCheck, IsPaperRole) and IsPaperRole then
          inc(ContainsPaperRole);
      end;

      FWareHouseFloor.SetData(WareHouseTile.Key, ContainsPaperRole);
      if ContainsPaperRole < 4 then
        FPaperRolesToRemove.Enqueue(WareHouseTile.Key);
    end;

  Result := FPaperRolesToRemove.Count;
end;

function TAdventOfCodeDay4.SolveB: Variant;
var
  CurrentPaperRolePosition, positionToCheck: TPosition;
  i: integer;
  CurrentNeighborCount: byte;
begin
  Result := 0;

  while FPaperRolesToRemove.Count > 0 do
  begin
    inc(result);
    CurrentPaperRolePosition := FPaperRolesToRemove.Dequeue;

    for i := 0 to 7 do
      begin
        positionToCheck := CurrentPaperRolePosition.ApplyDirections(DirectionsAround[i], 1);
        if FWareHouseFloor.TryGetValue(positionToCheck, CurrentNeighborCount) then
        begin
          if CurrentNeighborCount = 4 then
            FPaperRolesToRemove.Enqueue(positionToCheck);
          if CurrentNeighborCount >= 4 then
            FWareHouseFloor.SetData(positionToCheck, CurrentNeighborCount-1);
        end;
      end;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay5'}
procedure TAdventOfCodeDay5.BeforeSolve;
begin
  inherited;
  FAllRanges := TList<Range>.Create;
end;

procedure TAdventOfCodeDay5.AfterSolve;
begin
  inherited;
  FAllRanges.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
var
  s: string;
  ingredientId: int64;
  FillingRanges: Boolean;
  Split: TStringDynArray;
  r: Range;
begin
  Result := 0;
  FillingRanges := true;

  for s in FInput do
  begin
    if s = '' then
    begin
      FillingRanges := False;
      Continue
    end;

    if FillingRanges then
    begin
      Split := SplitString(s, '-');
      r.LowBound := split[0].ToInt64;
      r.HighBound := split[1].ToInt64;
      FAllRanges.Add(r);
      Continue;
    end;

    ingredientId := s.ToInt64;
    for r in FAllRanges do
      if InRange(ingredientId, r.LowBound, r.HighBound) then
      begin
        Inc(Result);
        Break;
      end;
  end;
end;

function TAdventOfCodeDay5.SolveB: Variant;
var
  normalisedRanges: TList<Range>;

  procedure AddToNormalisedRanges(aNewRange: Range);
  var
    OtherRangeIndex: integer;
    OtherRange: Range;
  begin
    for OtherRangeIndex := normalisedRanges.Count-1 downTo 0 do
    begin
      OtherRange := normalisedRanges[OtherRangeIndex];
      // Check if this range fits completely in the other range
      if (aNewRange.LowBound >= OtherRange.LowBound) and (aNewRange.HighBound <= OtherRange.HighBound) then
          Exit;

      // Check if the other range fits completely in this range
      if (aNewRange.LowBound < OtherRange.LowBound) and (aNewRange.HighBound > OtherRange.HighBound) then
      begin
        normalisedRanges.Delete(OtherRangeIndex);
        continue;
      end;

      if InRange(aNewRange.LowBound, OtherRange.LowBound, OtherRange.HighBound) then
      begin
        aNewRange.LowBound := OtherRange.LowBound;
        normalisedRanges.Delete(OtherRangeIndex);
      end;

      if InRange(aNewRange.HighBound, OtherRange.LowBound, OtherRange.HighBound) then
      begin
        aNewRange.HighBound := OtherRange.HighBound;
        normalisedRanges.Delete(OtherRangeIndex);
      end;
    end;

    normalisedRanges.Add(aNewRange)
  end;

var
  r: Range;
begin
  Result := 0;
  normalisedRanges := TList<Range>.Create;
  try
    for r in FAllRanges do
      AddToNormalisedRanges(r);

    for r in normalisedRanges do
      Inc(Result, r.HighBound - r.LowBound + 1);
  finally
    normalisedRanges.Free;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.CalculateSum(Operation: char; Numbers: TList<int64>): int64;
var
  i: integer;
begin
  Result := Numbers[0];
  Numbers[0] := 0;

  for i := 1 to Numbers.Count-1 do
  begin
    case IndexText(Operation, ['+', '*']) of
      0: inc(Result, Numbers[i]);
      1: Result := Result * Numbers[i];
    else
      raise Exception.Create('Invalid operation');
    end;
    numbers[i] := 0;
  end;
end;

function TAdventOfCodeDay6.SolveA: Variant;
var
  Nums: TList<int64>;
  TotalNums, i, numIndex: integer;
  Operation, c: char;
begin
  Result := 0;
  TotalNums := FInput.Count -1;
  Nums := TList<int64>.Create;
  for numIndex := 0 to TotalNums-1 do
    Nums.Add(0);

  Operation := '?';
  for i := 1 to Length(FInput[0]) do
  begin
    c := FInput[TotalNums][i];
    if c <> ' ' then
    begin
      if Operation <> '?' then
        inc(Result, CalculateSum(Operation, Nums));
      Operation := c;
    end;

    for numIndex := 0 to TotalNums-1 do
    begin
      c := FInput[numIndex][i];
      if c <> ' ' then
        nums[numIndex] := 10 * nums[numIndex] + StrToInt(c)
    end;
  end;

  inc(Result, CalculateSum(Operation, nums));
  nums.Free;
end;

function TAdventOfCodeDay6.SolveB: Variant;
var
  temp: int64;
  Nums: TList<int64>;
  TotalNums, i, numIndex: integer;
  Operation, c: char;
begin
  Result := 0;
  TotalNums := FInput.Count -1;
  Nums := TList<int64>.Create;
  Operation := '?';

  for i := 1 to Length(FInput[0]) do
  begin
    c := FInput[TotalNums][i];
    if c <> ' ' then
    begin
      if Operation <> '?' then
        inc(Result, CalculateSum(Operation, Nums));
      nums.Clear;
      Operation := c;
    end;

    temp := 0;
    for numIndex := 0 to TotalNums-1 do
    begin
      c := FInput[numIndex][i];
      if c <> ' ' then
        temp := 10 * temp + StrToInt(c)
    end;
    if temp > 0 then
      nums.Add(temp);
  end;

  inc(Result, CalculateSum(Operation, nums));
  nums.Free;
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
begin

end;

function TAdventOfCodeDay.SolveB: Variant;
begin
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3,TAdventOfCodeDay4,TAdventOfCodeDay5,
  TAdventOfCodeDay6]);

end.
