unit Services.ADB.Win;

interface

uses
  System.Classes;

procedure ExecCmdine(const CmdLine: string; CmdResult: TStrings);

implementation

uses
  Winapi.Windows, FMX.Forms;

procedure ExecCmdine(const CmdLine: string; CmdResult: TStrings);
const
    READ_BUFFER_SIZE = 2400;
var
    Security: TSecurityAttributes;
    readableEndOfPipe, writeableEndOfPipe: THandle;
    start: TStartUpInfo;
    ProcessInfo: TProcessInformation;
    Buffer: PAnsiChar;
    BytesRead: DWORD;
    AppRunning: DWORD;
begin
  Security.nLength := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;

  if CreatePipe({var}readableEndOfPipe, {var}writeableEndOfPipe, @Security, 0) then
  begin
    Buffer := AllocMem(READ_BUFFER_SIZE+1);
    FillChar(Start, Sizeof(Start), #0);
    start.cb := SizeOf(start);

    start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
    start.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    start.hStdOutput := writeableEndOfPipe;
    start.hStdError := writeableEndOfPipe;

    start.dwFlags := start.dwFlags + STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;

    ProcessInfo := Default(TProcessInformation);

    var LCmd := CmdLine;
    UniqueString({var}LCmd);

    if CreateProcess(nil, PChar(LCmd), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, start, {var}ProcessInfo) then
    begin
      repeat
        Apprunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        Application.ProcessMessages;
      until (Apprunning <> WAIT_TIMEOUT);

      repeat
        BytesRead := 0;
        ReadFile(readableEndOfPipe, Buffer[0], READ_BUFFER_SIZE, {var}BytesRead, nil);
        Buffer[BytesRead]:= #0;
        OemToAnsi(Buffer,Buffer);
        CmdResult.Text := CmdResult.text + string(Buffer);
      until (BytesRead < READ_BUFFER_SIZE);
    end;
    FreeMem(Buffer);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(readableEndOfPipe);
    CloseHandle(writeableEndOfPipe);
  end;
end;

end.
