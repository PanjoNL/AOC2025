unit AOCBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, system.Diagnostics, ClipBrd, system.UITypes,
  uAocConfig, uAocTimer;

type
  TProcedureToRun = procedure of object;
  TFunctionToRun = function: Variant of object;
  TLoadOverridenTestData = procedure(aInput: TStrings) of object;

type TAdventOfCode = class(TPersistent)
  constructor Create(aConfig: TAOCConfig);
  destructor Destroy; override;
  protected
    FInput: TStrings;
    function SolveA: Variant; virtual;
    function SolveB: Variant; virtual;
    procedure BeforeSolve; virtual;
    procedure AfterSolve; virtual;
    function SaveFilePath: String;
    procedure WriteTimeToDebug(Const aFunctionName: string; Const aTime: Int64);
  private
    FConfig: TAocConfig;
    function InputFilePath: string;
    function MakeFilePath(const aFileName: String): string;
    function DayIndex: String;
    procedure DoProcedure(ProcedureToRun: TProcedureToRun; const aDisplayName: String);
    function DoFunction(FuntionToRun: TFunctionToRun; const aDisplayName: string): String;
    procedure LoadInput;
    procedure InternalSolve(Out SolutionA, SolutionB: string);
  public
  { Public declarations }
    procedure Solve;
    procedure Test(Out SolutionA, SolutionB: String; Const LoadOverridenTestData: TLoadOverridenTestData);
  end;

implementation

uses
  uAOCUtils;

function TAdventOfCode.DayIndex: String;
begin
  Result := AOCUtils.DayIndexFromClassName(Self.ClassName);
end;

constructor TAdventOfCode.Create(aConfig: TAocConfig);
begin
  FConfig := aConfig;
  Assert(Self.ClassName.StartsWith('TAdventOfCodeDay'), 'Classname should begin with TAdventOfCodeDay, followd by the dayindex');

  FInput := TStringList.Create;
  DoProcedure(LoadInput, 'LoadInput');
end;

destructor TAdventOfCode.Destroy;
begin
  FInput.Free;
  inherited;
end;

function TAdventOfCode.SaveFilePath: String;
begin
  Result := MakeFilePath('Solution');
end;

function TAdventOfCode.InputFilePath: string;
begin
  Result := MakeFilePath('Input')
end;

function TAdventOfCode.MakeFilePath(const aFileName: String): string;
begin
  result := Format('%s\%s%s.txt', [FConfig.InputFolder, aFileName, DayIndex])
end;

function TAdventOfCode.SolveA: Variant;
begin
  Result := ''
end;

function TAdventOfCode.SolveB: Variant;
begin
  Result := ''
end;

procedure TAdventOfCode.BeforeSolve;
begin
  // To be overriden
end;

procedure TAdventOfCode.AfterSolve;
begin
  // To be overriden
end;

procedure TAdventOfCode.WriteTimeToDebug(Const aFunctionName: string; Const aTime: Int64);
begin
  Writeln(Format('%s -> Time: %d %s', [aFunctionName, aTime, TimeIndicator[MicroSeconds]] ));
end;

procedure TAdventOfCode.DoProcedure(ProcedureToRun: TProcedureToRun; const aDisplayName: String);
var
  Timer: AOCTimer;
begin
  Timer := AOCTimer.Start;
  ProcedureToRun;
  WriteTimeToDebug(aDisplayName, Timer.ElapsedTime);
end;

function TAdventOfCode.DoFunction(FuntionToRun: TFunctionToRun; const aDisplayName: string): String;
var
  Timer: AOCTimer;
begin
  Timer := AOCTimer.Start;
  Result := VarToStr(FuntionToRun);
  WriteTimeToDebug(aDisplayName, Timer.ElapsedTime);
end;

procedure TAdventOfCode.LoadInput;
var FilePath: string;
begin
  FilePath := InputFilePath;
  if FileExists(FilePath) then
    FInput.LoadFromFile(FilePath)
  else
  begin
    AOCUtils.DownLoadPuzzleInput(FInput, DayIndex, FConfig);
    FInput.SaveToFile(FilePath);
  end;
end;

procedure TAdventOfCode.Solve;

  procedure _ToClipBoard(Const aPart, aSolution: string);
  begin
    if (aSolution <> '') and (MessageDlg(Format('Solution %s: %s' +#10#13 + 'Copy to clipboard?', [aPart, aSolution]), mtInformation, [mbYes, mbNo], 0) = mrYes) then
      Clipboard.AsText := aSolution;
  end;

var
  SolutionA, SolutionB: String;
begin
  InternalSolve(SolutionA, SolutionB);

  _ToClipBoard('A', SolutionA);
  _ToClipBoard('B', SolutionB);
end;

procedure TAdventOfCode.InternalSolve(Out SolutionA, SolutionB: String);
begin
  DoProcedure(BeforeSolve, 'BeforeSolve');
  SolutionA := DoFunction(SolveA, 'SolveA');
  SolutionB := DoFunction(SolveB, 'SolveB');
  DoProcedure(AfterSolve, 'AfterSolve');
end;

procedure TAdventOfCode.Test(Out SolutionA, SolutionB: String; Const LoadOverridenTestData: TLoadOverridenTestData);
begin
  if Assigned(LoadOverridenTestData) then
    LoadOverridenTestData(FInput);

  InternalSolve(SolutionA, SolutionB);
end;

end.
