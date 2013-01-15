from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName(_fromUtf8("MainWindow"))
        MainWindow.resize(800, 756)
        self.centralwidget = QtGui.QWidget(MainWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.gridLayout = QtGui.QGridLayout(self.centralwidget)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.tableWidget = QtGui.QTableWidget(self.centralwidget)
        self.tableWidget.setColumnCount(3)
        self.tableWidget.setObjectName(_fromUtf8("tableWidget"))
        self.tableWidget.setRowCount(0)
        self.LockButton = QtGui.QPushButton(self.centralwidget)
        self.LockButton.setText("Lock")
        self.UnlockButton = QtGui.QPushButton(self.centralwidget)
        self.UnlockButton.setText("Unlock")
        self.gridLayout.addWidget(self.tableWidget, 0, 0, 1, 1)
        self.gridLayout.addWidget(self.LockButton, 2, 2, 2, 2)
        self.gridLayout.addWidget(self.UnlockButton, 2, 1, 1, 1)
        MainWindow.setCentralWidget(self.centralwidget)

        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)
        QtCore.QObject.connect(self.LockButton, QtCore.SIGNAL("clicked()"), MainWindow.lock)
        QtCore.QObject.connect(self.UnlockButton, QtCore.SIGNAL("clicked()"), MainWindow.unlock)

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(QtGui.QApplication.translate("MainWindow", "Distrex PyQt tester", None, QtGui.QApplication.UnicodeUTF8))
