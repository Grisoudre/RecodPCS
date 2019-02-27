# RecodPCS

*Recodage d'une variable selon la nomenclature PCS de l'Insee*

L'application permet d'importer n'importe quel tableau de données, de choisir la variable (de professions) qu'on souhaite recoder et facilite ensuite la recherche des métiers pour leur associer les libellés et codes PCS (Insee) correspondants. Le tableau initial augmenté des variables PCS peut être téléchargé de même que le recodage peut se faire en plusieurs fois, via l'option de sauvegarde.

## Installation et démarrage

**Installation du package :**
```{r}
if (!require("devtools")) install.packages("devtools", dep=T)
devtools::install_github("Grisoudre/RecodPCS")
```

**Ouverture de l'application :**
```{r}
library(RecodPCS)
PCS()
```
Puis, sur la nouvelle fenêtre, cliquer sur "Open in browser".

## Démonstration

![](https://github.com/Grisoudre/RecodPCS/blob/master/RecodPCS2.gif)

## Guide d'utilisation

**1er onglet - Importation :**

1. Importation du tableau
2. Choix de la variable à recoder
3. Choix du mode de recodage : 
    + Soit démarrage du recodage
    + Soit reprise d'une table de recodage sauvegardée

**2ème onglet - Recodage :**
- Tableau de recodage
- Champ de recherche
- Champs automatiques
- Sauvegarde de la table de recodage (pour poursuivre ultérieurement)

**3ème onglet - Table finale :**
- Aperçu de la table avec les variables PCS
- Téléchargement de la table finale

## Contenu

### Existant

**Fonction :**

PCS()

**Données :**

  - CSP_TousNiveaux : Ensemble des libellés et quatres niveaux de PCS correspondants.
  - ExempleRecodPCS : Table fictive d'exemple
