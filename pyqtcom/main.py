import time
import sys
import socket

import random

from PyQt4 import QtCore, QtGui
from PyQt4.QtCore import SIGNAL, QObject

from mainwindow_UI import Ui_MainWindow

class Heartbeat(QtCore.QThread):
    def __init__(self, resource_id):
        QtCore.QThread.__init__(self)
        self.resource_id = resource_id
        self.time_to_quit = False
        self.lock = QtCore.QMutex()

    def run(self):
        while self.isAlive():
            self.emit(SIGNAL('beat'), "%s" % self.resource_id)
            time.sleep(3)

    def isAlive(self):
        return not self.time_to_quit

    def setFinished(self):
        self.lock.lock()
        self.time_to_quit = True
        self.lock.unlock()

    def __str__(self):
        return "%s - %s" % (self.isRunning(), self.resource_id)

class LockState(object):
    def __init__(self, resource_id, state):
        self.resource_id = resource_id
        self.state = state

class GUI(QtGui.QMainWindow):
    def __init__(self, parent=None):
        QtGui.QMainWindow.__init__(self, parent)
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.socket.settimeout(1)
        self.socket.connect(("127.0.0.1", 8789))
        self.gui = Ui_MainWindow()
        self.gui.setupUi(self)
        self.heartbeats = {}
        self.display_state()

    def flush(self):
        try:
            while True:
                print self.socket.recv(1024)
        except socket.timeout:
            pass

    def unlock(self):
        print "unlocking..."
        self.flush()
        for k, v in self.heartbeats.items():
            self.unlock_resource(k)
        self.display_state()

    def lock(self):
        print "locking..."
        self.flush()
        for x in range(1000):
            self.lock_resource(x)
        self.display_state()

    def lock_resource(self, what):
        if self.heartbeats.get(what):
            return
        self.socket.send("CHECK%s" % what)
        resp = self.socket.recv(1024)
        if resp == "free":
            self.socket.send("LOCK%s" % what)
            if self.socket.recv(1024) == "ok":
                t = Heartbeat(what)
                QObject.connect(t, SIGNAL('beat'), self.do_beat, QtCore.Qt.QueuedConnection)
                t.start()
                self.heartbeats[what] = (t, LockState(what, True))
            else:
                print "unable to lock: %s" % what
        else:
            print "unable to lock: %s / %s " % (what, resp)

    def unlock_resource(self, what):
        if not self.heartbeats.get(what):
            return
        t = self.heartbeats[what][0]
        self.connect(t, SIGNAL("finished()"), lambda what=what: self.removeHeartbeat(what))
        t.setFinished()

    def removeHeartbeat(self, what):
        self.heartbeats.pop(what)
        self.socket.send("UNLOCK%s" % what)
        self.display_state()

    def display_state(self):
        self.clearTable()
        items = self.heartbeats.items()
        x = 0
        for (key, value) in items:
            self.gui.tableWidget.insertRow(x)
            self.gui.tableWidget.setItem(x, 0, QtGui.QTableWidgetItem(str(key)))
            self.gui.tableWidget.setItem(x, 1, QtGui.QTableWidgetItem(str(value[0])))
            self.gui.tableWidget.setItem(x, 2, QtGui.QTableWidgetItem(str(value[1].state)))
            x += 1

    def do_beat(self, what):
        self.socket.send("BEAT%s" % what)

    def clearTable(self):
        self.gui.tableWidget.setRowCount(0)

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    MAINWINDOW = GUI()
    MAINWINDOW.show()
    sys.exit(app.exec_())
