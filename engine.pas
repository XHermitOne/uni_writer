{
Модуль классов движка

Версия: 0.0.3.2
}

unit engine;


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    XmlRpcServer, XmlRpcTypes,
    dictionary, settings, obj_proto;

{ Режимы запуска движка }
const
  RUN_MODE_SINGLE: AnsiString = 'single';
  RUN_MODE_LOOP: AnsiString = 'loop';
  RUN_MODE_DIAGNOSTIC: AnsiString = 'diagnostic';

  DEFAULT_XML_RPC_PORT: Integer = 8081;

type
    {
    TICWriterProto - абстрактный тип движка
    ПИСАТЕЛЯ ДАННЫХ из различных источников
    }
    TICWriterProto = class(TObject)
    private
      { Менеджер настроек }
      FSettingsManager: TICSettingsManager;
      { Словарь зарегистрированных объектов }
      FObjects: TStrDictionary;

      { Сервер удаленного вызова процедур }
      FRpcServer: TRpcServer;
    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      {
      Проинициализировать конфигурационные переменные в соответствии с настройками
      @return True/False
      }
      function InitSettings(): Boolean;
      {
      Регистрация нового объекта в словаре внутренних объектов. Регистрация производиться по имени объекта.
      @param Obj Регистрируемый объект
      @return True -  регистрация прошла успешно / False - ошибка
      }
      function RegObject(Obj: TICObjectProto): Boolean;
      {
      Поиск объекта в зарегистрированных по имени
      @param sObjName Наименование объекта
      @return Найденный объект или nil если объект не найден среди зарегистрированных
      }
      function FindObject(sObjName: AnsiString): TICObjectProto;
      {
      Метод создания объекта контроллера данных с инициализацией его свойств
      @param Properties Словаряь свойств объекта
      @return Созданный объект или nil в случае ошибки
      }
      function CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;

      {
      Создание объектов по именам
      @param ObjectNames Список имен объектов
      @return Список созданных объектов
      }
      function CreateDataControllers(ObjectNames: TStringList=nil): TList;

    end;

    {
    TICWriter - Движок ПИСАТЕЛЯ ДАННЫХ из различных источников
    }
    TICWriter = class(TICWriterProto)
    private

    public
      { Конструктор }
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      {
      Записать значение в приемник данных
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в приемнике данных в строковом виде
      @param sValue Записываемое значение в виде строки
      @return True/False
      }
      function WriteValueAsString(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; sValue: AnsiString): Boolean;

      {
      Записать значение в приемник данных как целого числа (2 байта)
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в приемнике данных в строковом виде
      @param iValue Записываемое значение в виде целого числа
      @return True/False
      }
      function WriteValueAsInt2(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; iValue: Integer): Boolean;

      {
      Записать значение в приемник данных как целого числа (4 байта)
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в приемнике данных в строковом виде
      @param iValue Записываемое значение в виде целого числа
      @return True/False
      }
      function WriteValueAsInt4(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; iValue: Integer): Boolean;

      {
      Записать значение в приемник данных как логического значения
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в приемнике данных в строковом виде
      @param bValue Записываемое значение в виде логического значения
      @return True/False
      }
      function WriteValueAsBoolean(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; bValue: Boolean): Boolean;

      {
      Записать значение в приемник данных как вещественного значения
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в приемнике данных в строковом виде
      @param fValue Записываемое значение в виде вещественного значения
      @return True/False
      }
      function WriteValueAsFloat(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; fValue: Double): Boolean;

      {
      Записать список значений в приемник данных
      @param sDstTypeName Наименование типа приемника
      @param aArgs Массив дополнительных аргументов
      @param aAddresses Массив строк записываемых адресов
      @param aValues Список строк записываемых значений
      @return Список результатов записей
      }
      function WriteValuesAsStrings(sDstTypeName: AnsiString; const aArgs: Array Of Const; aAddresses: Array Of String; aValues: Array Of String): TStringList;

      { Запуск сервера движка/Инициализировать методы удаленного вызова }
      procedure StartServer;
      { Останов сервера движка }
      procedure StopServer;

      { --- Используемые процедуры удаленного вызова --- }
      { Тестовая функция для проверки удаленного вызова процедур }
      procedure EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                  List: TList; Return: TRpcReturn);

      { Функция записи данных в приемник удаленного вызова процедур }
      procedure WriteValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                            List: TList; Return: TRpcReturn);

      procedure WriteValueAsInt2RpcMethod(Thread: TRpcThread; const sMethodName: string;
                                          List: TList; Return: TRpcReturn);

      procedure WriteValueAsInt4RpcMethod(Thread: TRpcThread; const sMethodName: string;
                                          List: TList; Return: TRpcReturn);

      procedure WriteValueAsBooleanRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                             List: TList; Return: TRpcReturn);

      procedure WriteValueAsFloatRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                             List: TList; Return: TRpcReturn);

      { Функция записи данных в приемник удаленного вызова процедур }
      procedure WriteValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                              List: TList; Return: TRpcReturn);
    end;

var
  { Порт по умолчанию для обработки XML RPC }
  XML_RPC_PORT: Integer = 8081;

  {
  Объявление глобального объекта движка

  ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
  Переменные определенные в секции implementation являются статическими для
  модуля.
  }
  WRITER_ENGINE: TICWriter = nil;


implementation

uses
  log, config, reg_data_ctrl, strfunc;

constructor TICWriterProto.Create(TheOwner: TComponent);
begin
  inherited Create;

  FRpcServer := nil;

  // Менеджер настроек
  FSettingsManager := TICSettingsManager.Create;

  // Словарь зарегистрированных объектов
  FObjects := TStrDictionary.Create;

end;

destructor TICWriterProto.Destroy;
begin
  // ВНИМАНИЕ! Из Destroy необходимо вызывать Free.
  // В Free не должно быть вызова inherited Free.
  // Тогда не происходит утечки памяти
  Free;
  inherited Destroy;
end;

procedure TICWriterProto.Free;
begin
  if FRpcServer <> nil then
    FRpcServer.Destroy;
  // ВНИМАНИЕ! Из Destroy необходимо вызывать Free.
  // В Free не должно быть вызова inherited Free.
  // Тогда не происходит утечки памяти
  FObjects.Destroy;
  FSettingsManager.Destroy;
  inherited Free;
end;

{
Проинициализировать конфигурационные переменные в соответствии с настройками.
@return True/False
}
function TICWriterProto.InitSettings():Boolean;
var
  ini_filename: AnsiString;
begin
    if not ENVIRONMENT.HasKey('SETTINGS_FILENAME') then
    begin
       ini_filename := FSettingsManager.GenIniFileName();
       ENVIRONMENT.AddStrValue('SETTINGS_FILENAME', ini_filename);
    end
    else
        ini_filename := ENVIRONMENT.GetStrValue('SETTINGS_FILENAME');

    log.DebugMsgFmt('INI Файл <%s>', [ini_filename]);
    if (ini_filename <> '') and (not FileExists(ini_filename)) then
    begin
        log.WarningMsgFmt('Файл настроек <%s> не найден. Используется файл настроек по умолчанию', [ini_filename]);
        ini_filename := '';
    end;
    result := FSettingsManager.LoadSettings(ini_filename);

    FSettingsManager.PrintSettings;
end;

{
Регистрация нового объекта в словаре внутренних объектов.
Регистрация производиться по имени объекта.
@param Obj Регистрируемый объект
@return True -  регистрация прошла успешно / False - ошибка
}
function TICWriterProto.RegObject(Obj: TICObjectProto): Boolean;
var
  name: AnsiString;
begin
    if not obj.IsUnknown then
    begin
        // Регистрация по имени
        name := obj.GetName();
        FObjects.AddObject(name, obj);
        result := True;
        exit;
    end
    else
        log.WarningMsgFmt('Не возможно зарегистрировать объект класса <%s>', [obj.ClassName]);
    result := False;
end;

{
Поиск объекта в зарегистрированных по имени.
}
function TICWriterProto.FindObject(sObjName: AnsiString): TICObjectProto;
begin
    if FObjects.HasKey(sObjName) then
        result := FObjects.GetByName(sObjName) As TICObjectProto;
    log.WarningMsgFmt('Объект <%s> не найден среди зарегистрированных %s', [sObjName, FObjects.GetKeysStr()]);
    result := nil;
end;

{
Метод создания объекта контроллера данных с инициализацией его свойств.
@param (Properties  Словарь свойств контроллера данных)
@return (Объект контроллера данных или nil в случае ошибки)
}
function TICWriterProto.CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;
var
   type_name, name: AnsiString;
   ctrl_obj: TICObjectProto;
begin
    // Сначала в любом случае определяем тип источника данных
    if Properties.HasKey('type') then
    begin
        type_name := Properties.GetStrValue('type');
        ctrl_obj := CreateRegDataCtrl(self, type_name, Properties);
        if ctrl_obj <> nil then
        begin
             // Регистрируем новый объект в словаре внутренних объектов
             RegObject(ctrl_obj);
             result := ctrl_obj;
             exit;
        end;
    end
    else
    begin
        name := Properties.GetStrValue('name');
        log.ErrorMsgFmt('Ошибка создания объекта источника данных. Не определен тип <%s>', [name]);
    end;
    result := nil;
end;

{
Создание объектов по именам
}
function TICWriterProto.CreateDataControllers(ObjectNames: TStringList): TList;
var
   ctrl_objects: TList;
   obj: TICObjectProto;
   obj_names_str: AnsiString;
   i: Integer;
   obj_properties: TStrDictionary;
   is_obj_names_options: Boolean;
begin
    log.InfoMsg('Создание объектов...');
    ctrl_objects := TList.Create;
    is_obj_names_options := False;
    if ObjectNames = nil then
    begin
         obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'objects');
         ObjectNames := ParseStrList(obj_names_str);
         is_obj_names_options := True;
    end;

    for i := 0 to ObjectNames.Count - 1 do
    begin
        obj_properties := FSettingsManager.BuildSection(ObjectNames[i]);

        // Создаем объекты источников данных
        obj := CreateDataCtrl(obj_properties);
        if obj <> nil then
            ctrl_objects.Add(obj)
    end;

    // Освободить память если мы выделяли
    if is_obj_names_options then
       ObjectNames.Free;

    result := ctrl_objects;
end;

constructor TICWriter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TICWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TICWriter.Free;
begin
  inherited Free;
end;

{ Записать значение в приемник данных }
function TICWriter.WriteValueAsString(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; sValue: AnsiString): Boolean;
var
  ctrl_obj: TICObjectProto;

begin
  Result := False;
  ctrl_obj := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    Result := ctrl_obj.WriteAddress(sAddress, sValue);
  except
    log.FatalMsgFmt('Ошибка записи значения <%s> по адресу <%s>', [sValue, sAddress]);
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Записать целое значение (2 байта) в приемник данных }
function TICWriter.WriteValueAsInt2(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; iValue: Integer): Boolean;
var
  ctrl_obj: TICObjectProto;

begin
  Result := False;
  ctrl_obj := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    Result := ctrl_obj.WriteAddressAsInt2(sAddress, iValue);
  except
    log.FatalMsgFmt('Ошибка записи значения <%d> по адресу <%s>', [iValue, sAddress]);
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Записать целое значение (4 байта) в приемник данных }
function TICWriter.WriteValueAsInt4(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; iValue: Integer): Boolean;
var
  ctrl_obj: TICObjectProto;

begin
  Result := False;
  ctrl_obj := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    Result := ctrl_obj.WriteAddressAsInt4(sAddress, iValue);
  except
    log.FatalMsgFmt('Ошибка записи значения <%d> по адресу <%s>', [iValue, sAddress]);
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;


{ Записать значение в приемник данных }
function TICWriter.WriteValueAsBoolean(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; bValue: Boolean): Boolean;
var
  ctrl_obj: TICObjectProto;

begin
  Result := False;
  ctrl_obj := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    Result := ctrl_obj.WriteAddressAsBoolean(sAddress, bValue);
  except
    log.FatalMsgFmt('Ошибка записи значения <%d> по адресу <%s>', [bValue, sAddress]);
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Записать значение в приемник данных }
function TICWriter.WriteValueAsFloat(sDstTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString; fValue: Double): Boolean;
var
  ctrl_obj: TICObjectProto;

begin
  Result := False;
  ctrl_obj := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    Result := ctrl_obj.WriteAddressAsFloat(sAddress, fValue);
  except
    log.FatalMsgFmt('Ошибка записи значения <%f> по адресу <%s>', [fValue, sAddress]);
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Записать список значений в приемнике данных }
function TICWriter.WriteValuesAsStrings(sDstTypeName: AnsiString; const aArgs: Array Of Const; aAddresses: Array Of String; aValues: Array Of String): TStringList;
var
  ctrl_obj: TICObjectProto;
  str_list: TStringList;
  write_result: Boolean;

begin
  Result := nil;
  ctrl_obj := nil;
  str_list := nil;

  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sDstTypeName, aArgs);
    write_result := ctrl_obj.WriteAddresses(aAddresses, aValues);
    if write_result then
      Result := ctrl_obj.GetWriteValues();
  except
    log.FatalMsg('Ошибка записи значений по адресам:');
  end;

  //if str_list <> nil then
  //  str_list.Free;
  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Инициализировать методы удаленного вызова }
procedure TICWriter.StartServer;
var
  MethodHandler: TRpcMethodHandler;

begin
  if not Assigned(FRpcServer) then
  begin
    FRpcServer := TRpcServer.Create;
    FRpcServer.ListenPort := XML_RPC_PORT;
    FRpcServer.EnableIntrospect := True;

    try
      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'tests.echoString';
      // ВНИМАНИЕ! В Lazarus необходимо указывать @ для связки события с обработчиком
      //                         V
      MethodHandler.Method := @EchoTestRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Just a simple test rpc example method';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValueAsString';
      MethodHandler.Method := @WriteValueAsStringRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write value as string to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValueAsInt2';
      MethodHandler.Method := @WriteValueAsInt2RpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write value as integer to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValueAsInt4';
      MethodHandler.Method := @WriteValueAsInt4RpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write value as integer to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValueAsBoolean';
      MethodHandler.Method := @WriteValueAsBooleanRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write value as boolean to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValueAsFloat';
      MethodHandler.Method := @WriteValueAsFloatRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write value as double to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      MethodHandler := TRpcMethodHandler.Create;
      MethodHandler.Name := 'destinations.WriteValuesAsStrings';
      MethodHandler.Method := @WriteValuesAsStringsRpcMethod;
      MethodHandler.Signature := 'string (string myval)';
      MethodHandler.Help := 'Write values as strings to data destination';
      FRpcServer.RegisterMethodHandler(MethodHandler);
      // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      FRpcServer.Active := True;
    except
      log.FatalMsg('Ошибка запуска XML RPC сервера');
    end;
  end;
end;

procedure TICWriter.StopServer;
begin
  FRpcServer.Active := False;
  FRpcServer.Destroy;
  FRpcServer := nil;
end;

{ Тестовая функция для проверки удаленного вызова процедур }
procedure TICWriter.EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                      List: TList; Return: TRpcReturn);
var
  Msg: string;

begin
  {The parameter list is sent to your method as a TList of parameters
   this must be casted to a parameter to be accessed. If a error occurs
   during the execution of your method the server will fall back to a global
   handler and try to recover in which case the stack error will be sent to
   the client}

  {grab the sent string}
  Msg := TRpcParameter(List[0]).AsString;

  log.DebugMsgFmt('Test echo. You just sent: <%s>', [Msg]);

  {return a message showing what was sent}
  Return.AddItem('UniWriter. You just sent: ' + Msg);
end;

{ Функция записи данных в приемник удаленного вызова процедур }
procedure TICWriter.WriteValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                                List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: Boolean;
  value: AnsiString;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;
  value := TRpcParameter(List[3]).AsString;

  opc_result := WriteValueAsString(dst_type_name, [opc_server_name], address, value);

  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;

procedure TICWriter.WriteValueAsInt2RpcMethod(Thread: TRpcThread; const sMethodName: string;
                                              List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: Boolean;
  value: Integer;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;
  value := TRpcParameter(List[3]).AsInteger;

  opc_result := WriteValueAsInt2(dst_type_name, [opc_server_name], address, value);

  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;

procedure TICWriter.WriteValueAsInt4RpcMethod(Thread: TRpcThread; const sMethodName: string;
                                              List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: Boolean;
  value: Integer;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;
  value := TRpcParameter(List[3]).AsInteger;

  opc_result := WriteValueAsInt4(dst_type_name, [opc_server_name], address, value);

  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;


procedure TICWriter.WriteValueAsBooleanRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                                 List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: Boolean;
  value: Boolean;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;
  value := TRpcParameter(List[3]).AsBoolean;

  opc_result := WriteValueAsBoolean(dst_type_name, [opc_server_name], address, value);

  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;

procedure TICWriter.WriteValueAsFloatRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                               List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  address: AnsiString;
  opc_result: Boolean;
  value: Double;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;
  address := TRpcParameter(List[2]).AsString;
  value := TRpcParameter(List[3]).AsFloat;

  opc_result := WriteValueAsFloat(dst_type_name, [opc_server_name], address, value);

  {return a message showing what was sent}
  Return.AddItem(opc_result);
end;

{ Функция записи данных в приемник удаленного вызова процедур }
procedure TICWriter.WriteValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
                                                  List: TList; Return: TRpcReturn);
var
  dst_type_name: AnsiString;
  opc_server_name: AnsiString;
  addresses: Array of String;
  opc_result: TStringList;
  values: Array of String;
  i: Integer;
  opc_array: IRpcArray;

begin
  dst_type_name := TRpcParameter(List[0]).AsString;
  opc_server_name := TRpcParameter(List[1]).AsString;

  opc_array := TRpcParameter(List[1]).AsArray;
  SetLength(addresses, opc_array.Count);
  for i := 0 to opc_array.Count do
  begin
    addresses[i] := TRpcParameter(opc_array[i]).AsString;
    log.DebugMsgFmt('Запись тега <tag%d>. Адрес <%s>', [i, addresses[i]]);
  end;

  opc_array := TRpcParameter(List[2]).AsArray;
  SetLength(values, opc_array.Count);
  for i := 0 to opc_array.Count do
  begin
    values[i] := TRpcParameter(opc_array[i]).AsString;
    log.DebugMsgFmt('Запись тега <tag%d>. Значение <%s>', [i, values[i]]);
  end;

  opc_result := WriteValuesAsStrings(dst_type_name, [opc_server_name], addresses, values);
  addresses := nil;
  values := nil;

  {return a message showing what was sent}
  if opc_result <> nil then
  begin
    for i := 0 to opc_result.Count - 1 do
      Return.AddItem(opc_result.Strings[i]);
    opc_result.Free;
    opc_result := nil;
  end;
end;

end.

