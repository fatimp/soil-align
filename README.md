# soil-align

Программа для выравнивания 3D изображений пористых сред. Для установки
склонировать этот репозиторий в `local-projects` в quicklisp и выполнить

``` lisp
(ql:quickload :soil-align)
(asdf:make :soil-align) ;; для получения бинарника
```

Для генерации документации:

``` lisp
(ql:quickload :codex)
(codex:document :soil-align)
```

Зависимости:

* [https://github.com/bbrister/SIFT3D](SIFT3D)
* [https://pypi.org/project/pynndescent](pynndescent)
* SBCL (другая реализация CL не подойдёт)

Был бы здесь gitlab pages, тут была бы ссылка на документацию
