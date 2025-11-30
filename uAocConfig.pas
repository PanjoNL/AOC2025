unit uAocConfig;

interface

uses
  Registry;

type TAOCConfig = Class
private
  FBaseUrl: string;
  FInputFolder: string;
  FGithubRepo: string;

  function SetupRegistry(AAccess: LongWord): TRegistry;
  function GetSessionCookie: String;
  procedure SetSessionCookie(Const aValue: string);
public
  constructor Create;

  property BaseUrl: String read FBaseUrl;
  property InputFolder: string read FInputFolder;
  property GithubRepo: string read FGithubRepo;
  property SessionCookie: string read GetSessionCookie write SetSessionCookie;
End;


implementation

uses
  inifiles, System.SysUtils, winapi.Windows, uAOCUtils;


constructor TAOCConfig.Create;
const Config: string = 'Config';
var Ini: TIniFile;
    Path: string;
begin
  Path := AOCUtils.GetAocIniFilePath('AocConfig.ini');
  Ini := TIniFile.Create(Path+PathDelim+'AocConfig.ini');
  try
    FBaseUrl := Ini.ReadString(Config, 'BaseUrl', '');
    FGithubRepo := Ini.ReadString(Config, 'GithubRepo', 'https://github.com/PanjoNL');
    FInputFolder := Ini.ReadString(Config, 'InputFolder', '');;
  finally
    Ini.Free;
  end;
end;

function TAOCConfig.SetupRegistry(AAccess: LongWord): TRegistry;
begin
  Result := TRegistry.Create(AAccess);
  Result.RootKey := HKEY_CURRENT_USER;
  Result.OpenKey('Software\AOC', true);
end;

procedure TAOCConfig.SetSessionCookie(const aValue: string);
begin
 With SetupRegistry(KEY_WRITE) do
  try
    if aValue = '' then
      DeleteKey('AOCSessionCookie')
    else
      WriteString('AOCSessionCookie', aValue);
  finally
    Free;
  end;
end;

function TAOCConfig.GetSessionCookie: String;
begin
  With SetupRegistry(KEY_READ) do
  try
    Result := GetDataAsString('AOCSessionCookie');
  finally
    Free;
  end;
end;

end.
