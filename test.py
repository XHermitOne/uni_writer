#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Тестирование UniWriter.
"""

import unittest
import os
import xmlrpclib


class UniWriterTest(unittest.TestCase):
    """
    Класс тестирования UniWriter.
    """

    @classmethod
    def setUpClass(cls):
        """
        Метод выполняется перед выполнением всех тестов.
        """
        # Производим инсталляцию службы
        cmd = 'uni_writer.exe --install'
        os.system(cmd)

        # Запуск службы
        cmd = 'uni_writer.exe start'
        os.system(cmd)

    @classmethod
    def tearDownClass(cls):
        """
        Метод выполняется после выполнения всех тестов.
        """
        # Останов службы
        cmd = 'uni_writer.exe stop'
        os.system(cmd)

        # Производим деинсталляцию службы
        cmd = 'uni_writer.exe --uninstall'
        os.system(cmd)

    # ВНИМАНИЕ! 
    # Индекс в имени метода теста ставим для определения порядка выполнения тестов
    #          v 
    def test_0001_echo(self):
        """
        Простой тест проверки связи со службой.
        """
        srv = xmlrpclib.ServerProxy('http://localhost:8081')
        
        # Отобразисть список методов
        methods = srv.system.listMethods()
        for method_name in methods:
            print('Method: ', method_name)

        response = srv.tests.echoString('BlahBlahBlah')
        self.assertEqual(response, 'UniWriter. You just sent: BlahBlahBlah')


if __name__ == '__main__':
    unittest.main()

