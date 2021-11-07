#  UBA - Maestria en Explotación de Datos y Descubrimiento de Conocimiento - Datamining en Economia y Finanzas - Churn Prediction


## TP 2

[dmeyf2021segunda](https://www.kaggle.com/c/dmeyf2021segunda/leaderboard)

## Actualizar fork

1. Argegar repo  orignal como **upstream**:

```bash
$ git remote add upstream https://github.com/dmecoyfin/dmeyf.git
$ git remote -v
origin	https://github.com/magistery-tps/dmeyf.git (fetch)
origin	https://github.com/magistery-tps/dmeyf.git (push)
upstream	https://github.com/dmecoyfin/dmeyf.git (fetch)  <-- Repo registrado
upstream	https://github.com/dmecoyfin/dmeyf.git (push)   <-- Repo registrado
```

2. Traerse todo los branches del repo upstream:

```bash
$ git fetch upstream
```

3. Cambiar al branch master

```bash
$ git checkout master
```

4. Reescribir el branch master del fork con os comin del repo original y los comun que que se encuentren en el fork pero no en el repo original.

```bash
$ git rebase upstream/master
```
