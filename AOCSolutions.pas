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
  System.Threading, System.SyncObjs, system.Hash,
  Vcl.Imaging.pngimage, Vcl.Graphics;

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

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    ResultA, ResultB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  private
    ResultA, ResultB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    ResultA, ResultB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TMachine = class
    ButtonList: TList<TList<integer>>;
    Buttons, DesiredJolts: TList<integer>;
    DesiredLights: integer;
    ButtonCount: integer;
  public
    constructor Create(s: string);
    destructor Destroy; override;

    function ConfigureIndicatorLights: integer;
    function ConfigureJoltageIndicators: integer;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    Machines: TList<TMachine>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    Paths: TDictionary<word, TList<word>>;
    function FindPath(const aFrom, aTo: string): int64;
    function CreateKey(const aKey: string; CharCountToRemove: integer =0): word;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
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
{$REGION 'TAdventOfCodeDay7'}
procedure TAdventOfCodeDay7.BeforeSolve;
var
  Grid: TAocGrid<char>;
  Cache: TDictionary<int64, int64>;

  function WalkGrid(aCurrentPos: TPosition): int64;
  var
   c: char;
  begin
    aCurrentPos.AddDelta(0, 1);

    if not Grid.TryGetValue(aCurrentPos, c) then
      exit(1);

    if c = '.' then
      exit(WalkGrid(aCurrentPos));

    if Cache.TryGetValue(aCurrentPos.CacheKey, Result) then
      Exit;

    Result := WalkGrid(aCurrentPos.Clone.AddDelta(-1, 0)) + WalkGrid(aCurrentPos.Clone.AddDelta(1, 0));
    Cache.Add(aCurrentPos.CacheKey, Result);
  end;

var
  GridPosition: TPair<TPosition, char>;
begin
  Grid := TAocGridHelper.CreateCharGrid(FInput, False);
  Cache := TDictionary<int64, int64>.Create;
  ResultB := 0;

  for GridPosition in Grid do
    if GridPosition.Value = 'S' then
    begin
      ResultB := WalkGrid(GridPosition.Key);
      Break;
    end;

  ResultA := Cache.Count;
  Grid.Free;
  Cache.Free;
end;

function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
type
  TLight = class
    x,y,z: int64;
    Key: integer;
    ClosestLight: PriorityQueue<TPair<int64, TLight>>;

    constructor Create(aX, aY, aZ: int64; aKey: integer);
    procedure CalculateDistanceToOtherLights(aOtherLights: TList<TLight>);
  end;

  TDistanceComparer = class (TInterfacedObject, IComparer<TPair<int64, TLight>>)
    function Compare(const Left, Right: TPair<int64, TLight>): Integer;
  end;

constructor TLight.Create(aX, aY, aZ: int64; aKey: integer);
begin
  self.x := aX;
  self.y := aY;
  self.z := aZ;
  self.Key := aKey;

  self.ClosestLight := PriorityQueue<TPair<int64, TLight>>.Create(TDistanceComparer.Create, TDistanceComparer.Create);
end;

procedure TLight.CalculateDistanceToOtherLights(aOtherLights: TList<TLight>);
var
  i: Integer;
  OtherLight: TLight;
  distance: int64;
begin
  for i := 0 to self.Key -1 do
  begin
    OtherLight := aOtherLights[i];

    distance :=
      (Self.x - OtherLight.x) * (Self.x - OtherLight.x) +
      (Self.y - OtherLight.y) * (Self.y - OtherLight.y) +
      (Self.z - OtherLight.z) * (Self.z - OtherLight.z);

    ClosestLight.Enqueue(TPair<int64, TLight>.Create(Distance, OtherLight));
  end
end;

{ TDistanceComparer }

function TDistanceComparer.Compare(const Left, Right: TPair<int64, TLight>): Integer;
begin
  Result := Sign(Left.Key - Right.Key);
end;

procedure TAdventOfCodeDay8.BeforeSolve;
var
  ConnectedLights: TDictionary<integer, TList<TLight>>;

  function CalculateResultA(): int64;
  var
    Totals: TList<int64>;
    LightGroup: TList<TLight>;
  begin
    Totals := TList<int64>.Create;
    for LightGroup in ConnectedLights.Values do
      Totals.Add(LightGroup.Count);
    Totals.Sort;
    Result := Totals[Totals.Count-1] * Totals[Totals.Count-2] * Totals[Totals.Count-3];
    Totals.Free;
  end;

var
  s: string;
  split: TStringDynArray;
  AllLights, ConnectedLightList: TList<TLight>;
  LightQueue: PriorityQueue<TPair<int64, TLight>>;
  CurrentLight, Light1, Light2: TLight;
  i: integer;
begin
  AllLights := TObjectList<TLight>.Create(True);
  ConnectedLights := TObjectDictionary<integer, TList<TLight>>.Create([doOwnsValues]);
  LightQueue := PriorityQueue<TPair<int64, TLight>>.Create(TDistanceComparer.Create, TDistanceComparer.Create);

  try
    for s in FInput do
    begin
      Split := SplitString(s, ',');
      CurrentLight := TLight.Create(Split[0].ToInteger, Split[1].ToInteger, Split[2].ToInteger, ConnectedLights.Count);

      ConnectedLightList := TList<TLight>.Create;
      ConnectedLightList.Add(CurrentLight);
      ConnectedLights.Add(CurrentLight.Key, ConnectedLightList);

      AllLights.Add(CurrentLight);

      CurrentLight.CalculateDistanceToOtherLights(AllLights);
      if CurrentLight.ClosestLight.Count > 0 then
        LightQueue.Enqueue(TPair<int64, TLight>.Create(CurrentLight.ClosestLight.Peek.Key, CurrentLight));
    end;

    i := 0;
    while True do
    begin
      inc(i);

      if i = 1000 then
        ResultA := CalculateResultA;

      Light1 := LightQueue.Dequeue.Value;
      Light2 := Light1.ClosestLight.Dequeue.Value;

      if Light1.ClosestLight.Count > 0  then
        LightQueue.Enqueue(TPair<int64, TLight>.Create(Light1.ClosestLight.Peek.Key, Light1));

      if Light1.Key = Light2.Key then
        Continue;

      if ConnectedLights.Count = 2 then
      begin
        ResultB := Light1.x * Light2.x;
        Exit;
      end;

      ConnectedLightList := ConnectedLights.ExtractPair(Light1.Key).Value;
      for CurrentLight in ConnectedLightList do
        CurrentLight.Key := Light2.Key;
      ConnectedLights[Light2.Key].AddRange(ConnectedLightList);
      ConnectedLightList.Free;
    end;
  finally
    AllLights.Free;
    ConnectedLights.Free;
  end;
end;

function TAdventOfCodeDay8.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay8.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay9'}
procedure TAdventOfCodeDay9.BeforeSolve;
var
  s: string;
  split: TStringDynArray;
  r, i, j, minX, maxX, minY, MaxY, p3minX, p3maxX, p3minY, p3MaxY: integer;
  Points: TList<TPoint>;
  p1, p2, p3, p3Prev: TPoint;
  x, y, Area: int64;
  IsValid: Boolean;
begin
  ResultA := 0;
  ResultB := 0;

  Points := TList<TPoint>.Create;
  for s in FInput do
  begin
    split := SplitString(s, ',');
    Points.Add(TPoint.Create(Split[0].ToInteger, Split[1].ToInteger));
  end;
  Points.Add(Points.First);

  for i := 0 to Points.Count-3 do
    for j := i+1 to Points.Count-2 do
    begin
      p1 := Points[i];
      p2 := Points[j];

      minX := Min(p1.X, p2.x);
      maxX := Max(p1.X, p2.x);
      minY := Min(p1.y, p2.y);
      MaxY := Max(p1.y, p2.y);

      x := 1 + MaxX - MinX;
      y := 1 + MaxY - MinY ;

      Area :=  x * y;

      ResultA := Max(ResultA, Area);
      if Area < ResultB then
        Continue;

      IsValid := True;
      for r := 1 to points.Count-1 do
      begin
        p3Prev := Points[r-1];
        p3 := Points[r];

        p3minX := Min(p3Prev.X, p3.x);
        p3maxX := Max(p3Prev.X, p3.x);
        p3minY := Min(p3Prev.y, p3.y);
        p3MaxY := Max(p3Prev.y, p3.y);

        if (p3Prev.X = p3.x) and InRange(p3.x, MinX+1, MaxX-1)  then
          IsValid := (P3MaxY <= MinY) or (P3MinY >= MaxY)
        else if (p3Prev.y = p3.y) and InRange(p3.y, minY+1, MaxY-1) then
          IsValid := (P3MaxX <= MinX) or (P3MinX >= MaxX);

        if not IsValid then
          Break;
      end;

      if IsValid then
        ResultB := Area;
    end;
  Points.Free;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := ResultA;
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := ResultB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay10'}
type

  TButtonSorter = class (TInterfacedObject, IComparer<TList<integer>>)
    ButtonCounts: IntegerArray;
    function Compare(const Left, Right: TList<integer>): Integer;
    function DetermineRarestButtonCount(aButtonList: TList<integer>): integer;
    function CountRareButtons(aButtonList: TList<integer>; aRareCount: integer): integer;
  end;

{
LoadInput -> Time: 252 탎
BeforeSolve -> Time: 1183 탎
SolveA -> Time: 939 탎
Calculating machine 1 of 197
res 105
Calculating machine 2 of 197
res 273
Calculating machine 3 of 197
res 227
Calculating machine 4 of 197
res 63
Calculating machine 5 of 197
res 150
Calculating machine 6 of 197
res 60
Calculating machine 7 of 197
res 74
Calculating machine 8 of 197
res 93
Calculating machine 9 of 197
res 63
Calculating machine 10 of 197
res 194
Calculating machine 11 of 197
res 164
Calculating machine 12 of 197
res 30
Calculating machine 13 of 197
res 198
Calculating machine 14 of 197
res 36
Calculating machine 15 of 197
res 154
Calculating machine 16 of 197
res 195
Calculating machine 17 of 197
res 57
Calculating machine 18 of 197
res 253
Calculating machine 19 of 197
res 48
Calculating machine 20 of 197
res 76
Calculating machine 21 of 197
res 98
Calculating machine 22 of 197
res 279
Calculating machine 23 of 197
res 109
Calculating machine 24 of 197
res 26
Calculating machine 25 of 197
res 39
Calculating machine 26 of 197
res 226
Calculating machine 27 of 197
res 27
Calculating machine 28 of 197
res 113
Calculating machine 29 of 197
res 88
Calculating machine 30 of 197
res 72
Calculating machine 31 of 197
res 138
Calculating machine 32 of 197
res 79
Calculating machine 33 of 197
res 59
Calculating machine 34 of 197
res 95
Calculating machine 35 of 197
res 87
Calculating machine 36 of 197
res 82
Calculating machine 37 of 197
res 213
Calculating machine 38 of 197
res 47
Calculating machine 39 of 197
res 243
Calculating machine 40 of 197
res 176
Calculating machine 41 of 197
res 59
Calculating machine 42 of 197
res 75
Calculating machine 43 of 197
res 217
Calculating machine 44 of 197
res 69
Calculating machine 45 of 197
res 103
Calculating machine 46 of 197
res 63
Calculating machine 47 of 197
res 177
Calculating machine 48 of 197
res 138
Calculating machine 49 of 197
res 110
Calculating machine 50 of 197
res 51
Calculating machine 51 of 197
res 49
Calculating machine 52 of 197
res 94
Calculating machine 53 of 197
res 91
Calculating machine 54 of 197
res 132
Calculating machine 55 of 197
res 79
Calculating machine 56 of 197
res 48
Calculating machine 57 of 197
res 21
Calculating machine 58 of 197
res 153
Calculating machine 59 of 197
res 88
Calculating machine 60 of 197
res 18
Calculating machine 61 of 197
res 58
Calculating machine 62 of 197
res 63
Calculating machine 63 of 197
res 49
Calculating machine 64 of 197
res 31
Calculating machine 65 of 197
res 250
Calculating machine 66 of 197
res 145
Calculating machine 67 of 197
res 120
Calculating machine 68 of 197
res 86
Calculating machine 69 of 197
res 56
Calculating machine 70 of 197
res 205
Calculating machine 71 of 197
res 106
Calculating machine 72 of 197
res 168
Calculating machine 73 of 197
res 117
Calculating machine 74 of 197
res 70
Calculating machine 75 of 197
res 65
Calculating machine 76 of 197
res 70
Calculating machine 77 of 197
res 257
Calculating machine 78 of 197
res 124
Calculating machine 79 of 197
res 91
Calculating machine 80 of 197
res 23
Calculating machine 81 of 197
res 28
Calculating machine 82 of 197
res 31
Calculating machine 83 of 197
res 108
Calculating machine 84 of 197
res 75
Calculating machine 85 of 197
res 84
Calculating machine 86 of 197
res 33
Calculating machine 87 of 197
res 69
Calculating machine 88 of 197
res 154
Calculating machine 89 of 197
res 55
Calculating machine 90 of 197
res 77
Calculating machine 91 of 197
res 217
Calculating machine 92 of 197
res 30
Calculating machine 93 of 197
res 198
Calculating machine 94 of 197
res 25
Calculating machine 95 of 197
res 29
Calculating machine 96 of 197
res 48
Calculating machine 97 of 197
res 139
Calculating machine 98 of 197
res 84
Calculating machine 99 of 197
res 82
Calculating machine 100 of 197
res 125
Calculating machine 101 of 197
res 48
Calculating machine 102 of 197
res 138
Calculating machine 103 of 197
res 235
Calculating machine 104 of 197
res 47
Calculating machine 105 of 197
res 45
Calculating machine 106 of 197
res 47
Calculating machine 107 of 197
res 209
Calculating machine 108 of 197
res 163
Calculating machine 109 of 197
res 86
Calculating machine 110 of 197
res 18
Calculating machine 111 of 197
res 85
Calculating machine 112 of 197
res 248
Calculating machine 113 of 197
res 305
Calculating machine 114 of 197
res 106
Calculating machine 115 of 197
res 213
Calculating machine 116 of 197
res 56
Calculating machine 117 of 197
res 241
Calculating machine 118 of 197
res 85
Calculating machine 119 of 197
res 187
Calculating machine 120 of 197
res 195
Calculating machine 121 of 197
res 103
Calculating machine 122 of 197
res 42
Calculating machine 123 of 197
res 111
Calculating machine 124 of 197
res 181
Calculating machine 125 of 197
res 19
Calculating machine 126 of 197
res 47
Calculating machine 127 of 197
res 227
Calculating machine 128 of 197
res 49
Calculating machine 129 of 197
res 48
Calculating machine 130 of 197
res 49
Calculating machine 131 of 197
res 100
Calculating machine 132 of 197
res 68
Calculating machine 133 of 197
res 16
Calculating machine 134 of 197
res 189
Calculating machine 135 of 197
res 78
Calculating machine 136 of 197
res 42
Calculating machine 137 of 197
res 88
Calculating machine 138 of 197
res 225
Calculating machine 139 of 197
res 53
Calculating machine 140 of 197
res 31
Calculating machine 141 of 197
res 80
Calculating machine 142 of 197
res 46
Calculating machine 143 of 197
res 254
Calculating machine 144 of 197
res 209
Calculating machine 145 of 197
res 106
Calculating machine 146 of 197
res 209
Calculating machine 147 of 197
res 113
Calculating machine 148 of 197
res 251
Calculating machine 149 of 197
res 69
Calculating machine 150 of 197
res 131
Calculating machine 151 of 197
res 37
Calculating machine 152 of 197
res 74
Calculating machine 153 of 197
res 117
Calculating machine 154 of 197
res 220
Calculating machine 155 of 197
res 217
Calculating machine 156 of 197
res 51
Calculating machine 157 of 197
res 79
Calculating machine 158 of 197
res 45
Calculating machine 159 of 197
res 241
Calculating machine 160 of 197
res 271
Calculating machine 161 of 197
res 56
Calculating machine 162 of 197
res 197
Calculating machine 163 of 197
res 53
Calculating machine 164 of 197
res 56
Calculating machine 165 of 197
res 110
Calculating machine 166 of 197
res 291
Calculating machine 167 of 197
res 60
Calculating machine 168 of 197
res 178
Calculating machine 169 of 197
res 149
Calculating machine 170 of 197
res 42
Calculating machine 171 of 197
res 28
Calculating machine 172 of 197
res 23
Calculating machine 173 of 197
res 45
Calculating machine 174 of 197
res 60
Calculating machine 175 of 197
res 53
Calculating machine 176 of 197
res 15
Calculating machine 177 of 197
res 224
Calculating machine 178 of 197
res 255
Calculating machine 179 of 197
res 48
Calculating machine 180 of 197
res 94
Calculating machine 181 of 197
res 62
Calculating machine 182 of 197
res 61
Calculating machine 183 of 197
res 209
Calculating machine 184 of 197
res 14
Calculating machine 185 of 197
res 61
Calculating machine 186 of 197
res 31
Calculating machine 187 of 197
res 191
Calculating machine 188 of 197
res 75
Calculating machine 189 of 197
res 209
Calculating machine 190 of 197
res 85
Calculating machine 191 of 197
res 260
Calculating machine 192 of 197
res 166
Calculating machine 193 of 197
res 70
Calculating machine 194 of 197
res 81
Calculating machine 195 of 197
res 115
Calculating machine 196 of 197
res 194
Calculating machine 197 of 197
res 71
SolveB -> Time: 1134404417 탎
AfterSolve -> Time: 125 탎
}



function TButtonSorter.CountRareButtons(aButtonList: TList<integer>; aRareCount: integer): integer;
var
  ButtonId: integer;
begin
  Result := 0;
  for ButtonId in aButtonList do
    if ButtonCounts[ButtonId] = aRareCount then
      Inc(Result);

end;

function TButtonSorter.DetermineRarestButtonCount(aButtonList: TList<integer>): integer;
var
  ButtonId: integer;
begin
  result := MaxInt;
  for ButtonId in aButtonList do
    Result := min(Result, ButtonCounts[ButtonId]);
end;

function TButtonSorter.Compare(const Left, Right: TList<integer>): Integer;
var
  MostRareLeft: integer;
begin
  MostRareLeft := DetermineRarestButtonCount(Left);

  Result := sign(MostRareLeft - DetermineRarestButtonCount(Right));

  if Result = 0 then
    Result := -sign(CountRareButtons(Left, MostRareLeft) - CountRareButtons(Right, MostRareLeft));

  if Result = 0 then
    Result := -Sign(Left.Count - Right.Count)
end;

constructor TMachine.Create(s: string);
var
  s2: string;
  split, split2: tStringDynArray;
  i, button: Integer;
  tmpButtons: TList<integer>;
begin
  ButtonList := TObjectList<TList<integer>>.Create;
  Buttons := TList<integer>.Create;
  DesiredJolts := TList<integer>.Create;
  DesiredLights := 0;

  Split := SplitString(s, ' ');
  for i := 2 to Length(split[0])-1 do
    if split[0][i] = '#' then
      DesiredLights := DesiredLights + 1 shl (i-2);

  for i := 1 to Length(split) -2 do
  begin
    split2 := SplitString(Copy(Split[i], 2, Split[i].Length-2) , ',');

    button := 0;
    tmpButtons := TList<integer>.Create;
    for s2 in split2 do
    begin
      button := button + 1 shl (s2.ToInteger());
      tmpButtons.Add(s2.ToInteger);
    end;

    Buttons.Add(button);
    ButtonList.Add(tmpButtons);
  end;

  i := Length(split) -1;
  split2 := SplitString(Copy(Split[i], 2, Split[i].Length-2) , ',');
  for s2 in split2 do
    DesiredJolts.Add(s2.ToInteger);
  ButtonCount := DesiredJolts.Count;
end;

destructor TMachine.Destroy;
begin
  ButtonList.Free;
  Buttons.Free;
  DesiredJolts.Free;
end;

function TMachine.ConfigureIndicatorLights: integer;

  function UseSwitch(const currentIdx, currentLight, buttonPresses: integer): integer;
  begin
    if currentLight = DesiredLights then
      Exit(buttonPresses);

    if currentIdx > Buttons.Count-1 then
      exit(maxInt);

    Result := min(
      UseSwitch(currentIdx + 1, currentLight, buttonPresses),
      UseSwitch(currentIdx + 1, currentLight xor buttons[currentIdx], buttonPresses + 1));
  end;

begin
  Result := UseSwitch(0,0,0);
end;

function TMachine.ConfigureJoltageIndicators: integer;
var
  ButtonSet: TList<integer>;
  FinalButtons: IntegerArray;
  ButtonCounts: IntegerArray;
  ButtonId: integer;
  ButtonSorter: TButtonSorter;
  i, j: integer;
  BestResult: integer;
  CurrentJoltage: Array of integer;
//  ButtonsPressed: Array of integer;
  ButtonArray: Array of TArray<integer>;

  function IsFinalButton(aFromRow, aButtonId: integer): Boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := aFromRow to ButtonList.Count-1 do
      if ButtonList[i].Contains(aButtonId) then
        Exit(false);
  end;

//  procedure WriteButtonStat(aButtonId: integer);
//  var s: string;
//  i: integer;
//  begin
//    s := '(' ;
//    for i in ButtonList[aButtonId] do
//      s := s + i.ToString +  ' ';
//    s := s + ')';
//
//    WriteLn(ButtonsPressed[aButtonId], ' -> ', s);
//  end;

  function Calculate(aPresses, currentButtonIdx: integer): integer;
  var
    MinNoOfPressesNeeded, i: integer;
    Ok: Boolean;
    ButtonId: Integer;
    MinNoOfPressesToDo, MaxNoOfPressesToDo: integer;
  begin
    Ok := True;
    MinNoOfPressesNeeded := 0;
    Result := MaxINt;

    for i := 0 to ButtonCount-1 do
    begin
      ok := ok and (CurrentJoltage[i] = 0);

      if CurrentJoltage[i] < 0 then
        Exit(MaxInt);

      MinNoOfPressesNeeded := Max(MinNoOfPressesNeeded, CurrentJoltage[i]);
    end;

    if Ok then
    begin
//      WriteLn('Found in ', aPresses);
//      for I := 0 to ButtonList.Count-1 do
//        WriteButtonStat(i);

      BestResult := min(BestResult, aPresses);
      Exit(BestResult);
    end;

    if currentButtonIdx > ButtonList.Count-1 then
      Exit(MaxInt);

    if (aPresses + MinNoOfPressesNeeded) >= BestResult  then
      Exit(MaxInt);


    MinNoOfPressesToDo := 0;
    MaxNoOfPressesToDo := MaxInt;
    for ButtonId in ButtonArray[currentButtonIdx] do
    begin
      if ((FinalButtons[currentButtonIdx] shr buttonId) and 1) = 1 then
      begin
        MinNoOfPressesToDo := CurrentJoltage[ButtonId];
        MaxNoOfPressesToDo := MinNoOfPressesToDo;
        break;
      end;

      MaxNoOfPressesToDo := min(MaxNoOfPressesToDo, CurrentJoltage[ButtonId]);
    end;

    if MinNoOfPressesToDo < 0 then
      Exit(MaxInt);

    Assert(MaxNoOfPressesToDo <> MaxInt);
    Assert(MinNoOfPressesToDo >= 0);

    for i := MaxNoOfPressesToDo downto MinNoOfPressesToDo do
    begin
      for ButtonId in ButtonArray[currentButtonIdx] do
        CurrentJoltage[ButtonId] := CurrentJoltage[ButtonId] - i;
//      ButtonsPressed[currentButtonIdx] := i;
      Result := min(Result, Calculate(aPresses + i, currentButtonIdx + 1));
      for ButtonId in ButtonArray[currentButtonIdx] do
        CurrentJoltage[ButtonId] := CurrentJoltage[ButtonId] + i;
    end;
  end;

var s: string;

begin
  // Order buttons based on the most rare digit
  SetLength(ButtonCounts, ButtonCount);
  for ButtonSet in ButtonList do
    for ButtonId in ButtonSet do
      ButtonCounts[ButtonId] := ButtonCounts[ButtonId] + 1;

  ButtonSorter := TButtonSorter.Create;
  ButtonSorter.ButtonCounts := ButtonCounts;
  ButtonList.Sort(ButtonSorter);

  SetLength(FinalButtons, ButtonList.Count);
  SetLength(ButtonArray, ButtonList.Count);
  for i := 0 to ButtonList.Count -1 do
  begin
    // Convert buttons to an array for faster indexing;
    ButtonArray[i] := ButtonList[i].ToArray;

    // Determine if this is the last time a button is seen in the orderd buttons
    for buttonId := 0 to desiredJolts.Count-1 do
      if IsFinalButton(i+1, ButtonId) then
        FinalButtons[i] := FinalButtons[i] + 1 shl ButtonId;
  end;

  SetLength(CurrentJoltage, DesiredJolts.Count);
  for i := 0 to ButtonCount -1 do
    CurrentJoltage[i] := DesiredJolts[i];

  BestResult := MaxInt;

//  SetLength(ButtonsPressed, Buttons.Count);




//  WriteLn('SortedButtons');
//  for i := 0 to ButtonList.Count-1 do
//  begin
//    s := '';
//    for j := 0 to ButtonList[i].Count-1 do
//      s := s + ',' + ButtonList[i][j].ToString;
//    WriteLn(s);
//  end;

  Result := Calculate(0,0);














end;

procedure TAdventOfCodeDay10.BeforeSolve;
var
  s: string;
begin
//  FInput.Clear;
//  FInput.Add('[.###...##.] (8) (0,3,4,5,6,7,8,9) (0,4,5,9) (0,1,2,3,5,7,8) (2,3,4,5,6,7,9) (1,2,3,6,7,9) (0,1,2,3,4,5,7,8) (2,3,5,8,9) (6,8,9) (1,2,3,4,5,6,7,9) (1,4,6,8,9) (0,3,6,9) (0,2,3,7) {65,50,69,97,72,86,95,82,68,117}');

//  FInput.Add('[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}');
//  FInput.Add('[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}');
//  FInput.Add('[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}');

  inherited;
  Machines := TObjectList<TMachine>.Create();
  for s in FInput do
    Machines.Add(TMachine.Create(s));
end;

procedure TAdventOfCodeDay10.AfterSolve;
begin
  inherited;
  Machines.Free;
end;

function TAdventOfCodeDay10.SolveA: Variant;
var
  Machine: TMachine;
begin
  result := 0;
  for Machine in Machines do
    inc(Result, Machine.ConfigureIndicatorLights);
end;

function TAdventOfCodeDay10.SolveB: Variant;
var
  Machine: TMachine;
  i, j: integer;
  TotalResult: Integer;
begin
  result := 0;
//  i := 0;
//  for Machine in Machines do
//  begin
//    inc(i);
//    WriteLn('Calculating machine ', i, ' of ', Machines.Count);
//    j := Machine.ConfigureJoltageIndicators;
//    WriteLn('res ', j);
//    inc(Result, j);
//  end;

//  var
//  Result: Integer;
//begin

  TotalResult := 0;

  TParallel.For(0, Machines.Count - 1,
    procedure(i: Integer)
    var
      r: Integer;
    begin
      r := Machines[i].ConfigureJoltageIndicators;
      TInterlocked.Add(TotalResult, r);
    end
  );

  Result := TotalResult;

end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
procedure TAdventOfCodeDay11.BeforeSolve;
var
  s: string;
  i: integer;
  Split: TStringDynArray;
  tmpPaths: TList<word>;
begin
  inherited;
  Paths := TObjectDictionary<word, TList<word>>.Create([doOwnsValues]);
  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    tmpPaths := TList<word>.Create;
    Paths.Add(CreateKey(Split[0], 1), tmpPaths);
    for i := 1 to Length(Split)-1 do
      tmpPaths.Add(CreateKey(Split[i]));
  end;
end;

function TAdventOfCodeDay11.CreateKey(const aKey: string; CharCountToRemove: integer): word;
const
  a: integer = ord('a');
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(aKey) - CharCountToRemove do
    Result := (Result shl 5) + ord(aKey[i]) - a;
end;

procedure TAdventOfCodeDay11.AfterSolve;
begin
  inherited;
  Paths.Free;
end;

function TAdventOfCodeDay11.FindPath(const aFrom, aTo: string): int64;
var
  Seen: TDictionary<integer, int64>;

  function InternalCalcPath(const aFrom, aTo: word): int64;
  var
    tmpPaths: TList<word>;
    nextNode: word;
  begin
    if aFrom = aTo then
      Exit(1);

    if Seen.TryGetValue(afrom, Result) then
      Exit;

    Result := 0;
    if not Paths.TryGetValue(aFrom, tmpPaths) then
      Exit;

    for nextNode in tmpPaths do
      Result := Result + InternalCalcPath(nextNode, aTo);

    Seen.Add(aFrom, Result);
  end;

begin
  Seen := TDictionary<integer, int64>.Create;
  Result := InternalCalcPath(CreateKey(aFrom), CreateKey(aTo));
  Seen.Free;
end;

function TAdventOfCodeDay11.SolveA: Variant;
begin
  result := FindPath('you', 'out');
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result :=
    (FindPath('svr', 'fft') * FindPath('fft', 'dac') * FindPath('dac', 'out')) +
    (FindPath('svr', 'dac') * FindPath('dac', 'fft') * FindPath('fft', 'out'));
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
  TAdventOfCodeDay6,TAdventOfCodeDay7,TAdventOfCodeDay8,TAdventOfCodeDay9,TAdventOfCodeDay10,
  TAdventOfCodeDay11]);

end.
