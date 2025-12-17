unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows, system.Classes,
  uAocUtils, AocSolutions, AOCBase, uAOCConfig, uAocTimer;

type
  AOCTest = record
    AOCClass: TAdventOfCodeRef;
    ExpectedSolutionA, ExpectedSolutionB: String;
    LoadOverridenTestData: TLoadOverridenTestData
end;

type AOCTests = class
public
  Class procedure RunTests(aConfig: TAOCConfig);
end;

Const AOCTestData: array[0..11] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '1141'; ExpectedSolutionB: '6634'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '53420042388'; ExpectedSolutionB: '69553832684'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '17524'; ExpectedSolutionB: '173848577117276'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '1474'; ExpectedSolutionB: '8910'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '529'; ExpectedSolutionB: '344260049617193'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '5227286044585'; ExpectedSolutionB: '10227753257799'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '1533'; ExpectedSolutionB: '10733529153890'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '75680'; ExpectedSolutionB: '8995844880'),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '4774877510'; ExpectedSolutionB: '1560475800'),
 (AOCClass: TAdventOfCodeDay10; ExpectedSolutionA: '514'; ExpectedSolutionB: '21824'),
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '634'; ExpectedSolutionB: '377452269415704'),
 (AOCClass: TAdventOfCodeDay12; ExpectedSolutionA: '589'; ExpectedSolutionB: '')


);

implementation
class procedure AOCTests.RunTests(aConfig: TAOCConfig);

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
  end;

Var
  Test: AOCTest;
  AdventOfCode: TAdventOfCode;
  TotalTime, TestTimer: AocTimer;
  SolutionA, SolutionB: string;
  Times: TStringList;
  ElapsedMicroSeconds: Integer;
  s: string;
begin
  Writeln('');

  Times := TStringList.Create;
  try
    TotalTime := AOCTimer.Start;
    for Test in AOCTestData do
    begin
      Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

      AdventOfCode := Test.AOCClass.Create(aConfig);

      TestTimer := AOCTimer.Start;
      AdventOfCode.Test(SolutionA, SolutionB, Test.LoadOverridenTestData);
      ElapsedMicroSeconds := TestTimer.ElapsedTime;
      Times.Add(Format('%s -> Time: %d %s', [Test.AOCClass.Classname, ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      AdventOfCode.Free;

      _Check('Part a', Test.ExpectedSolutionA, SolutionA);
      _Check('Part b', Test.ExpectedSolutionB, SolutionB);
      Writeln(FormAt('Total time %d %s', [ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      Writeln('');
    end;

    Writeln(Format('All tests done in %d %s', [TotalTime.ElapsedTime(MilliSeconds), TimeIndicator[MilliSeconds]]));
    for s in Times do
      WriteLn(s);
  finally
    Times.Free;
  end
end;

end.
