{
Модуль обработчиков демона/службы
}
unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type

  { TUniWriterDaemon непосредственно экземпляр сервиса.
  Именно этот объект реализует сам сервис (его мы видим в Windows в «Управление компьютерами/Сервисы»).}

  TUniWriterDaemon = class(TDaemon)
    { Обработчик после инсталяции }
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    { Обработчик после деинсталяции }
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    { Обработчик запуска демона/службы }
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    { Обработчик останова демона/службы }
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private

  public

  end;

var
  UniWriterDaemon: TUniWriterDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TUniWriterDaemon)
end;

{$R *.lfm}

{ TUniWriterDaemon }

procedure TUniWriterDaemon.DataModuleAfterInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is installing');
end;

procedure TUniWriterDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is deinstalling');
end;

procedure TUniWriterDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  engine.WRITER_ENGINE := TICReader.Create(nil);
  //engine.WRITER_ENGINE.RegRpcMethods;
  engine.WRITER_ENGINE.StartServer;
end;

procedure TUniWriterDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  engine.WRITER_ENGINE.StopServer;
  engine.WRITER_ENGINE.Free;
  engine.WRITER_ENGINE := nil;
end;


initialization
  RegisterDaemon;
end.

