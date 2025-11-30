unit uAOCTimer;

interface

type
  TTimerType = (MicroSeconds, MilliSeconds);

  AOCTimer = record
  private
    FStart: Int64;
  public
    class function Start: AOCTimer; static;

    function ElapsedTime(aTimerType: TTimerType = MicroSeconds): integer;
    procedure WriteTimeToDebug(Const aMsg: String; aTimerType: TTimerType = MicroSeconds);
    procedure Reset;
end;

const
  TimeDivider: Array[TTimerType] of int64 = (1000000, 1000);
  TimeIndicator: Array[TTimerType] of string = ('µs', 'ms');

implementation

uses
  Winapi.Windows;

{ AOCTimer }

procedure AOCTimer.Reset;
begin
  QueryPerformanceCounter(FStart);
end;

class function AOCTimer.Start: AOCTimer;
begin
  Result.Reset;
end;

procedure AOCTimer.WriteTimeToDebug(const aMsg: String; aTimerType: TTimerType);
begin
  Writeln(aMsg, ' ', ElapsedTime(aTimerType), TimeIndicator[aTimerType]);
end;

function AOCTimer.ElapsedTime(aTimerType: TTimerType): Integer;
Var
  Stop, Freq, Units: int64;
begin
  QueryPerformanceCounter(Stop);
  QueryPerformanceFrequency(Freq);
  units := Freq div TimeDivider[aTimerType];
  Result := (Stop - FStart) div Units
end;

end.
