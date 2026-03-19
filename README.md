# soil-align

Программа для выравнивания 3D изображений пористых сред.

## Зависимости:

* [https://github.com/fatimp/SIFT3D](SIFT3D) version 2.0+
* [https://pypi.org/project/pynndescent](pynndescent)
* OpenBLAS
* LMDB
* SBCL (другая реализация CL не подойдёт)
* Qlot

## Установка

Для установки перейти в директорию, куда склонирован этот репозиторий и
выполнить

~~~~
$ qlot install
$ qlot exec sbcl --dynamic-space-size 100gb
~~~~

и в REPL ввести команду

``` lisp
(asdf:make :soil-align)
```

Получившийся бинарник забрать из `src`

``` lisp
(ql:quickload :codex)
(codex:document :soil-align)
```

Был бы здесь gitlab pages, тут была бы ссылка на документацию
