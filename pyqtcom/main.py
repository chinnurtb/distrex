import time
import sys
import socket

import random

from PyQt4 import QtCore, QtGui
from PyQt4.QtCore import SIGNAL, QObject

from freepoint.ui.mainwindow_UI import Ui_MainWindow

class Heartbeat(QtCore.QThread):
    def __init__(self, resource_id):
        QtCore.QThread.__init__(self)
        self.resource_id = resource_id

    def run(self):
        while 1:
            self.emit(SIGNAL('beat'), "%s" % self.resource_id)
            time.sleep(3)

class LockState(object):
    def __init__(self, resource_id, state):
        self.resource_id = resource_id
        self.state = state

class GUI(QtGui.QMainWindow):
    def __init__(self, parent=None):
        QtGui.QMainWindow.__init__(self, parent)
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.socket.connect(("127.0.0.1", 8789))
        self.gui = Ui_MainWindow()
        self.gui.setupUi(self)
        self.heartbeats = {}
        for x in range(10):
            self.lock_resource(random.choice(range(10)))
        self.display_state()

    def lock_resource(self, what):
        if self.heartbeats.get(what):
            return            
        self.heartbeats[what] = (None, LockState(what, False))
        self.socket.send("CHECK%s" % what)
        resp = self.socket.recv(1024)
        if resp == "free":
            self.socket.send("LOCK%s" % what)
            if  self.socket.recv(1024) == "ok":
                t = Heartbeat(what)
                QObject.connect(t, SIGNAL('beat'), self.do_beat, QtCore.Qt.QueuedConnection)
                t.start()
                self.heartbeats[what] = (t, LockState(what, True))
            else:
                print "unable to lock!"

    def display_state(self):
        for key, value in self.heartbeats.items():
            self.gui.tableWidget.insertRow(self.gui.tableWidget.rowCount())
            self.gui.tableWidget.setItem(self.gui.tableWidget.rowCount() -1, 0,
                                         QtGui.QTableWidgetItem(str(key)))
            self.gui.tableWidget.setItem(self.gui.tableWidget.rowCount() -1, 1,
                                         QtGui.QTableWidgetItem(str(value[1].state)))

    def do_beat(self, what):
        self.socket.send("BEAT%s" % what)

    def changeTable(self):
        pass
    def openConnectionDialog(self):
        pass
    def storeCell(self):
        pass
    def openManageDialog(self):
        pass

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    MAINWINDOW = GUI()
    MAINWINDOW.show()
    sys.exit(app.exec_())
