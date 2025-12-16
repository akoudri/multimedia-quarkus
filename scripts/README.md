# Administration Scripts

Scripts d'administration pour les microservices de la Bibliothèque Multimédia, suivant les meilleures pratiques de la méthodologie des **Douze Facteurs** (12-Factor App).

## Vue d'ensemble

```
scripts/
├── config.sh           # Configuration partagée et fonctions utilitaires
├── infra-up.sh         # Démarrer l'infrastructure (Docker)
├── infra-down.sh       # Arrêter l'infrastructure
├── dev.sh              # Lancer un service en mode développement
├── start-all.sh        # Démarrer tous les services
├── stop-all.sh         # Arrêter tous les services
├── health-check.sh     # Vérifier l'état de santé
├── status.sh           # Afficher le statut complet du système
├── logs.sh             # Consulter les logs
├── build.sh            # Compiler les services
├── clean.sh            # Nettoyer les artifacts
└── env-check.sh        # Vérifier l'environnement
```

## Principes 12-Factor appliqués

| Facteur | Description | Script(s) |
|---------|-------------|-----------|
| **II. Dependencies** | Déclarer et isoler les dépendances | `build.sh`, `clean.sh` |
| **III. Config** | Stocker la configuration dans l'environnement | `config.sh`, `env-check.sh` |
| **IV. Backing Services** | Traiter les services externes comme ressources | `infra-up.sh`, `infra-down.sh` |
| **V. Build, Release, Run** | Séparer strictement les étapes | `build.sh`, `start-all.sh` |
| **VIII. Concurrency** | Scalabilité via les processus | `start-all.sh` |
| **IX. Disposability** | Démarrage rapide et arrêt gracieux | `stop-all.sh` |
| **X. Dev/Prod Parity** | Parité développement/production | `dev.sh` |
| **XI. Logs** | Traiter les logs comme flux d'événements | `logs.sh` |
| **XII. Admin Processes** | Exécuter les tâches d'admin en processus ponctuels | Tous les scripts |

## Guide d'utilisation

### 1. Vérification de l'environnement

```bash
# Vérifier que tout est correctement configuré
./scripts/env-check.sh

# Mode verbose
./scripts/env-check.sh -v

# Tenter de corriger les problèmes
./scripts/env-check.sh --fix
```

### 2. Démarrage de l'infrastructure

```bash
# Démarrer toute l'infrastructure
./scripts/infra-up.sh

# Démarrer uniquement les bases de données
./scripts/infra-up.sh --only-db

# Démarrer uniquement l'observabilité (Prometheus, Grafana, Zipkin)
./scripts/infra-up.sh --only-observability
```

### 3. Développement d'un service

```bash
# Lancer le service catalog en mode dev (hot reload)
./scripts/dev.sh catalog

# Avec débogage distant
./scripts/dev.sh catalog --debug

# Sur un port différent
./scripts/dev.sh catalog -p 9081
```

### 4. Démarrage de tous les services

```bash
# Démarrer tous les services en mode production
./scripts/start-all.sh

# En mode développement
./scripts/start-all.sh -d

# Exclure certains services
./scripts/start-all.sh --skip notifications
```

### 5. Surveillance et santé

```bash
# Vérifier l'état de tous les services
./scripts/health-check.sh

# Mode surveillance continue
./scripts/health-check.sh -w

# Statut complet du système
./scripts/status.sh

# Statut court
./scripts/status.sh -s
```

### 6. Consultation des logs

```bash
# Voir les logs d'un service
./scripts/logs.sh catalog

# Suivre les logs en temps réel
./scripts/logs.sh -f catalog

# Voir les logs de tous les services
./scripts/logs.sh --all

# Filtrer les erreurs
./scripts/logs.sh catalog --errors

# Logs Docker
./scripts/logs.sh --docker kafka
```

### 7. Arrêt des services

```bash
# Arrêter tous les microservices
./scripts/stop-all.sh

# Forcer l'arrêt
./scripts/stop-all.sh -f

# Arrêter l'infrastructure
./scripts/infra-down.sh

# Arrêter et supprimer les volumes (données)
./scripts/infra-down.sh -v
```

### 8. Build et nettoyage

```bash
# Compiler tous les services
./scripts/build.sh

# Compiler avec nettoyage préalable
./scripts/build.sh -c

# Compiler avec tests
./scripts/build.sh -t

# Nettoyer les artifacts
./scripts/clean.sh

# Nettoyer tout (build, logs, docker)
./scripts/clean.sh -a
```

## Variables d'environnement

Les scripts utilisent les variables définies dans `config.sh` :

| Variable | Description | Valeur par défaut |
|----------|-------------|-------------------|
| `APP_ENV` | Environnement (dev/staging/prod) | `dev` |
| `PROJECT_ROOT` | Racine du projet | Auto-détecté |
| `LOG_DIR` | Répertoire des logs | `${PROJECT_ROOT}/logs` |
| `DOCKER_DIR` | Répertoire Docker | `${PROJECT_ROOT}/docker` |

## Ports des services

| Service | Port |
|---------|------|
| catalog | 8081 |
| users | 8082 |
| reviews | 8083 |
| reactive-reviews | 8084 |
| notifications | 8085 |

## Dépannage

### Le service ne démarre pas

```bash
# Vérifier si le port est déjà utilisé
./scripts/health-check.sh --services

# Vérifier les logs
./scripts/logs.sh <service> --errors
```

### L'infrastructure ne répond pas

```bash
# Vérifier l'état des conteneurs Docker
./scripts/status.sh

# Redémarrer l'infrastructure
./scripts/infra-down.sh && ./scripts/infra-up.sh
```

### Problèmes de mémoire

```bash
# Vérifier les ressources
./scripts/status.sh

# Nettoyer les ressources Docker
./scripts/clean.sh --docker
```

## Création de nouveaux scripts

Pour créer un nouveau script d'administration :

1. Sourcer la configuration : `source "${SCRIPT_DIR}/config.sh"`
2. Utiliser les fonctions de logging : `log_info`, `log_success`, `log_error`
3. Ajouter une section `usage()` pour l'aide
4. Supporter les options `-h/--help`
5. Implémenter un arrêt gracieux avec `set -euo pipefail`

Exemple :

```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]
...
EOF
    exit 0
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help) usage ;;
        *) shift ;;
    esac
done

# Main logic
log_step "Doing something..."
log_success "Done!"
```
