#  UBA - Maestria en Explotación de Datos y Descubrimiento de Conocimiento - Datamining en Economia y Finanzas - Churn Prediction


## TP 2

[dmeyf2021segunda](https://www.kaggle.com/c/dmeyf2021segunda/leaderboard)
[Resumne basic google cloud](https://github.com/magistery-tps/dmeyf/blob/master/docs/Resumen%20Google%20Cloud.pdf)

## Actualizar Fork

1. Registrar repo orignal como **upstream**:

```bash
$ git remote add upstream https://github.com/dmecoyfin/dmeyf.git

$ git remote -v
origin    https://github.com/magistery-tps/dmeyf.git (fetch)  <-- Nuestro fork
origin    https://github.com/magistery-tps/dmeyf.git (push)   <-- Nuestro fork
upstream  https://github.com/dmecoyfin/dmeyf.git (fetch)      <-- Repo original
upstreaM  https://github.com/dmecoyfin/dmeyf.git (push)       <-- Repo original
```

2. Traerse todos los branches(actualizados) del repo upstream:

```bash
$ git fetch upstream
```

3. Cambiar al branch master:

```bash
$ git checkout master
```

4. Reescribir el branch master del fork con los commits del repo original y los commits que se encuentren en el fork pero no en el repo original.

```bash
$ git rebase upstream/master
```

## Descargar dataset

```bash
$ git clone https://github.com/magistery-tps/dmeyf.git 
```



