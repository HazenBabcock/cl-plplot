
/*
#ifdef __SMOKEGEN_RUN__
#  define MYAPP_EXPORT
#endif

#define MYAPP_EXPORT
*/

#include <iostream>
#include <QImage>
#include <QPainter>
#include <QLinkedList>
#include <QPrinter>
#include <QApplication>
#include <QWidget>
#include <QMouseEvent>
#include <QTabWidget>
#include <QMainWindow>
#include <QPicture>
#include <QMutex>

class QtPLDriver
{
public:
QtPLDriver(int i_iWidth, int i_iHeight);
virtual ~QtPLDriver();             
};

class QtPLWidget : public QWidget, public QtPLDriver
{
Q_OBJECT

public:
QtPLWidget( int i_iWidth, int i_iHeight, QWidget * parent = 0 );
virtual ~QtPLWidget();
};

class QtExtWidget : public QtPLWidget
{
    Q_OBJECT

public:
QtExtWidget( int i_iWidth, int i_iHeight, QWidget * parent = 0 );
virtual ~QtExtWidget();
};
