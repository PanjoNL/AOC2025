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
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

implementation

{$REGION 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;

begin
  inherited;


end;

procedure TAdventOfCodeDay1.AfterSolve;
begin
  inherited;

end;

function TAdventOfCodeDay1.SolveA: Variant;
var
  s: string;
  split: TStringDynArray;
begin
  for s in FInput do
  begin
    split := SplitString(s, ',');
  end;
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1]);

end.
