#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Тестирование UniWriter.
"""

import unittest
import os
import os.path
import xmlrpclib

NOT_DEFINE_PRODUCT_CODE = 'DEF_ProductName_Code'
EXAMPLE1_PRODUCT_CODE = '0031923000001237391'
LOG_FILENAME = os.path.join(os.getcwd(), 'uni_writer.log')

class UniWriterTest(unittest.TestCase):
    """
    Класс тестирования UniWriter.
    """
    srv = None

    @classmethod
    def setUpClass(cls):
        """
        Метод выполняется перед выполнением всех тестов.
        Метод действует на уровне класса, т.е. выполняется
        перед запуском тестов класса. 
        При этом синтаксис требует наличие декоратора @classmethod.
        """
        # Удаляем лог службы
        if os.path.exists(LOG_FILENAME):
            os.remove(LOG_FILENAME)                
    
        # Производим инсталляцию службы
        cmd = 'uni_writer.exe --install'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

        # Запуск службы
        cmd = 'net start UniWriterGateway'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

    @classmethod
    def tearDownClass(cls):
        """
        Метод выполняется после выполнения всех тестов.
        Запускается после выполнения всех тестов класса,
        требует наличия декоратора @classmethod.
        """
        # Останов службы
        cmd = 'net stop UniWriterGateway'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

        # Производим деинсталляцию службы
        cmd = 'uni_writer.exe --uninstall'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)  

    def setUp(self):
        """
        Метод вызывается перед запуском теста.
        Как правило, используется для подготовки окружения  для теста.
        """
        self.srv = xmlrpclib.ServerProxy('http://localhost:8081')
        self.assertIsNotNone(self.srv)

    def tearDown(self):
        """
        Метод вызывается после завершения работы теста.
        Используется для 'приборки' за тестом.
        """
        self.srv = None

    # ВНИМАНИЕ! 
    # Индекс в имени метода теста ставим для определения порядка выполнения тестов
    #          v 
    def test_0001_echo(self):
        """
        Простой тест проверки связи со службой.
        """
        # Отобразисть список методов
        print(u'Список доступных методов:')
        methods = self.srv.system.listMethods()
        for method_name in methods:
            print('\tMethod: %s' % method_name)

        response = self.srv.tests.echoString('BlahBlahBlah')
        self.assertEqual(response, 'UniWriter. You just sent: BlahBlahBlah')

    def _test_0002_read_string(self):
        """
        Тест чтения строки из тега.
        """
        response = self.srv.destinations.WriteValueAsString('OPC_SERVER_NODE', 'RSLinx OPC Server', 
                                '[Absolute_Universal]LineData[1].ProdCodeNew', 
                                'DEF_ProductName_Code')
        self.assertEqual(response, True)

    def test_0003_write_string(self):
        """
        Тест записи строки в тег.
        """
        response = self.srv.destinations.WriteValueAsString('OPC_SERVER_NODE', 'RSLinx OPC Server', 
                                '[Absolute_Universal]LineData[1].ProdCodeNew', 
                                NOT_DEFINE_PRODUCT_CODE)
        self.assertEqual(response, True)

    def test_0004_write_integer(self):
        """
        Тест записи целого числа в тег.
        """
        #response = self.srv.destinations.WriteValueAsInteger('OPC_SERVER_NODE', 'RSLinx OPC Server', 
        #                                                     '[Absolute_Universal]LineData[1].ModeNew', 
        #                                                     0)
        response = self.srv.destinations.WriteValueAsString('OPC_SERVER_NODE', 'RSLinx OPC Server', 
                                                            '[Absolute_Universal]LineData[1].ModeNew', 
                                                            '000')
        self.assertEqual(response, True)


if __name__ == '__main__':
    unittest.main()

