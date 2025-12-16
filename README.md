# Bibliothèque Multimédia - Microservices Quarkus

Projet mono-repo Gradle contenant une architecture microservices complète pour la gestion d'une bibliothèque multimédia, développée avec Quarkus.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CLIENTS                                         │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            KEYCLOAK (IAM)                                    │
│                              :8180/auth                                      │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         MICROSERVICES                                        │
├─────────────┬─────────────┬─────────────┬─────────────────┬─────────────────┤
│   Catalog   │    Users    │   Reviews   │ Reactive Reviews│  Notifications  │
│   :8081     │    :8082    │    :8083    │     :8084       │     :8085       │
└──────┬──────┴──────┬──────┴──────┬──────┴────────┬────────┴────────┬────────┘
       │             │             │               │                 │
       ▼             ▼             ▼               ▼                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         INFRASTRUCTURE                                       │
├─────────────┬─────────────┬─────────────┬─────────────┬─────────────────────┤
│ PostgreSQL  │    Redis    │   Consul    │    Kafka    │    Observability    │
│  (x3 DBs)   │    :6379    │    :8500    │   :29092    │ Prometheus/Grafana  │
│ 5432/6543/  │   Cache     │  Discovery  │  Messaging  │ Zipkin/OTel         │
│    7654     │             │             │             │                     │
└─────────────┴─────────────┴─────────────┴─────────────┴─────────────────────┘
```

## Microservices

| Service | Port | Description | Base de données |
|---------|------|-------------|-----------------|
| **catalog** | 8081 | Gestion du catalogue de ressources (livres, DVD, etc.) | catalog (5432) |
| **users** | 8082 | Gestion des utilisateurs et authentification | users (7654) |
| **reviews** | 8083 | Gestion des avis et notations | reviews (6543) |
| **reactive-reviews** | 8084 | Version réactive du service reviews | reviews (6544) |
| **notifications** | 8085 | Service batch de notifications | users (7654) |

## Prérequis

- **Java 21** ou supérieur
- **Docker** et Docker Compose
- **Gradle 8.x** (wrapper inclus)

### Vérification de l'environnement

```bash
./scripts/env-check.sh
```

## Démarrage rapide

### 1. Démarrer l'infrastructure

```bash
# Démarrer tous les services d'infrastructure
./scripts/infra-up.sh

# Ou uniquement les bases de données
./scripts/infra-up.sh --only-db
```

### 2. Lancer un service en développement

```bash
# Démarrer le service catalog avec hot-reload
./scripts/dev.sh catalog

# Avec débogage distant (port 5005)
./scripts/dev.sh catalog --debug
```

### 3. Vérifier l'état du système

```bash
# Statut complet
./scripts/status.sh

# Vérification de santé
./scripts/health-check.sh
```

## Structure du projet

```
multimedia-quarkus/
├── catalog/                 # Service catalogue
├── users/                   # Service utilisateurs
├── reviews/                 # Service avis (impératif)
├── reactive-reviews/        # Service avis (réactif)
├── notifications/           # Service batch notifications
├── shared-config/           # Configuration partagée
├── docker/                  # Configuration Docker
│   ├── docker-compose.yml   # Infrastructure complète
│   ├── prometheus/          # Configuration Prometheus
│   ├── grafana/             # Dashboards Grafana
│   └── otel-collector-config.yaml
├── scripts/                 # Scripts d'administration
│   ├── infra-up.sh          # Démarrer l'infrastructure
│   ├── infra-down.sh        # Arrêter l'infrastructure
│   ├── dev.sh               # Mode développement
│   ├── start-all.sh         # Démarrer tous les services
│   ├── stop-all.sh          # Arrêter tous les services
│   ├── health-check.sh      # Vérifier la santé
│   ├── status.sh            # Statut du système
│   ├── logs.sh              # Consulter les logs
│   ├── build.sh             # Compiler les services
│   ├── clean.sh             # Nettoyer les artifacts
│   └── env-check.sh         # Vérifier l'environnement
├── build.gradle             # Configuration Gradle racine
├── settings.gradle          # Configuration multi-modules
└── gradle.properties        # Propriétés Gradle
```

## Infrastructure Docker

### Services disponibles

| Service | Port | URL |
|---------|------|-----|
| **Keycloak** | 8180 | http://localhost:8180/auth (admin/training) |
| **PostgreSQL Catalog** | 5432 | `jdbc:postgresql://localhost:5432/catalog` |
| **PostgreSQL Users** | 7654 | `jdbc:postgresql://localhost:7654/users` |
| **PostgreSQL Reviews** | 6543 | `jdbc:postgresql://localhost:6543/reviews` |
| **Redis** | 6379 | `redis://localhost:6379` |
| **Consul** | 8500 | http://localhost:8500 |
| **Kafka** | 29092 | `localhost:29092` |
| **Kafka UI** | 8080 | http://localhost:8080 |
| **Prometheus** | 9090 | http://localhost:9090 |
| **Grafana** | 3000 | http://localhost:3000 (admin/admin) |
| **Zipkin** | 9411 | http://localhost:9411 |
| **PgAdmin** | 80 | http://library.local/pgadmin4 (via Traefik) |
| **Traefik Dashboard** | 8090 | http://localhost:8090 |

### Gestion de l'infrastructure

```bash
# Démarrer
./scripts/infra-up.sh

# Arrêter
./scripts/infra-down.sh

# Arrêter et supprimer les volumes (⚠️ perte de données)
./scripts/infra-down.sh -v
```

## Authentification (Keycloak)

L'authentification est gérée par **Keycloak** via le protocole **OpenID Connect (OIDC)**.

### Configuration

| Paramètre | Valeur |
|-----------|--------|
| URL | http://localhost:8180/auth |
| Realm | `training` |
| Client ID | `quarkus-app` |
| Admin | `admin` / `training` |

### Utilisateurs de test

| Utilisateur | Mot de passe | Rôles |
|-------------|--------------|-------|
| alice | alice | user, admin |

### Intégration Quarkus

Les services utilisent `quarkus-oidc` pour la sécurisation :

```properties
# application.properties
quarkus.oidc.auth-server-url=http://localhost:8180/auth/realms/training
quarkus.oidc.client-id=quarkus-app
quarkus.oidc.application-type=web-app
quarkus.oidc.roles.source=realm
```

### Endpoints protégés (Catalog)

| Endpoint | Accès |
|----------|-------|
| `GET /web/catalog` | Public |
| `GET /web/catalog/{id}` | Public |
| `GET /web/catalog/new` | Rôle `admin` |
| `POST /web/catalog` | Rôle `admin` |
| `GET /web/catalog/{id}/edit` | Rôle `admin` |
| `POST /web/catalog/{id}` | Rôle `admin` |
| `POST /web/catalog/{id}/delete` | Rôle `admin` |

### Obtenir un token (API)

```bash
# Token pour alice
curl -X POST "http://localhost:8180/auth/realms/training/protocol/openid-connect/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=password" \
  -d "client_id=quarkus-app" \
  -d "username=alice" \
  -d "password=alice"
```

## Développement

### Compilation

```bash
# Compiler tous les services
./scripts/build.sh

# Compiler avec nettoyage
./scripts/build.sh -c

# Compiler un service spécifique
./scripts/build.sh catalog

# Compiler avec tests
./scripts/build.sh -t
```

### Mode développement (Hot Reload)

```bash
# Lancer un service
./scripts/dev.sh catalog

# Avec débogage
./scripts/dev.sh users --debug

# Sur un port différent
./scripts/dev.sh reviews -p 9083
```

### Tests

```bash
# Tous les tests
./gradlew test

# Tests d'un module
./gradlew :catalog:test

# Tests d'intégration
./gradlew :catalog:test --tests "*IT"
```

## Observabilité

### Tracing distribué (Zipkin)

Les traces sont automatiquement collectées via OpenTelemetry et envoyées à Zipkin.

- **Interface** : http://localhost:9411
- **Protocole** : OTLP → OTel Collector → Zipkin
- **Propagation** : W3C Trace Context

Les logs incluent les identifiants de trace :
```
2024-01-15 10:30:45 INFO traceId=abc123, spanId=def456 [catalog] Request received
```

### Métriques (Prometheus/Grafana)

Chaque service expose des métriques Micrometer au format Prometheus.

- **Endpoint** : `http://localhost:{port}/q/metrics`
- **Prometheus** : http://localhost:9090
- **Grafana** : http://localhost:3000

Métriques disponibles :
- JVM (heap, threads, GC)
- HTTP (requêtes, latence, codes de statut)
- Base de données (connexions, requêtes)
- Cache Redis

### Health Checks

```bash
# Vérifier tous les services
./scripts/health-check.sh

# Mode surveillance continue
./scripts/health-check.sh -w

# Détails verbose
./scripts/health-check.sh -v
```

Endpoints de santé :
- Liveness : `http://localhost:{port}/q/health/live`
- Readiness : `http://localhost:{port}/q/health/ready`

## Messaging (Kafka)

### Topics

| Topic | Producteur | Consommateurs |
|-------|------------|---------------|
| `library-resources` | catalog | users, reviews |
| `library-users` | users | reviews |
| `library-reviews` | reviews | catalog |

### Kafka UI

Interface de gestion : http://localhost:8080

## Service Discovery (Consul)

Les services s'enregistrent automatiquement dans Consul avec :
- Health check HTTP
- Métadonnées (version, environnement)

Interface Consul : http://localhost:8500

### Configuration Stork (REST Client)

Le service catalog utilise Stork pour la découverte de services :
```properties
quarkus.rest-client.reviews-service.url=stork://reviews-service
quarkus.rest-client.users-service.url=stork://users-service
```

## Scripts d'administration

Tous les scripts suivent les bonnes pratiques de la méthodologie **12-Factor App** et se trouvent dans le dossier `scripts/`.

### Vue d'ensemble

| Script | Description |
|--------|-------------|
| `env-check.sh` | Vérifier l'environnement de développement |
| `infra-up.sh` | Démarrer l'infrastructure Docker |
| `infra-down.sh` | Arrêter l'infrastructure |
| `dev.sh` | Lancer un service en mode développement |
| `dev-all.sh` | Lancer plusieurs services en mode développement |
| `start-all.sh` | Démarrer tous les microservices (production) |
| `stop-all.sh` | Arrêter gracieusement les services |
| `health-check.sh` | Vérifier l'état de santé |
| `status.sh` | Afficher le statut complet |
| `logs.sh` | Consulter les logs |
| `build.sh` | Compiler les services |
| `clean.sh` | Nettoyer les artifacts |

### Infrastructure (infra-up.sh / infra-down.sh)

```bash
# Démarrer toute l'infrastructure
./scripts/infra-up.sh

# Démarrer uniquement les bases de données
./scripts/infra-up.sh --only-db

# Démarrer uniquement Kafka
./scripts/infra-up.sh --only-messaging

# Démarrer uniquement la stack d'observabilité
./scripts/infra-up.sh --only-observability

# Démarrer des services spécifiques
./scripts/infra-up.sh postgres redis kafka

# Arrêter l'infrastructure
./scripts/infra-down.sh

# Arrêter et supprimer les volumes (⚠️ perte de données)
./scripts/infra-down.sh -v

# Arrêter avec suppression des containers orphelins
./scripts/infra-down.sh --remove-orphans
```

### Mode développement (dev.sh)

Lance un service unique en mode Quarkus Dev avec hot-reload.

```bash
# Démarrer le service catalog
./scripts/dev.sh catalog

# Avec débogage distant (port 5005)
./scripts/dev.sh users --debug

# Suspendre jusqu'à connexion du debugger
./scripts/dev.sh users --debug --suspend

# Sur un port personnalisé
./scripts/dev.sh reviews -p 9083

# Nettoyer avant de démarrer
./scripts/dev.sh catalog --clean
```

**Services disponibles** : `catalog` (8081), `users` (8082), `reviews` (8083), `reactive-reviews` (8084), `notifications` (8085)

### Mode développement multi-services (dev-all.sh)

Lance plusieurs services en mode développement, chacun dans sa propre fenêtre de terminal.

```bash
# Démarrer les services par défaut (catalog, users, reviews)
./scripts/dev-all.sh

# Démarrer uniquement catalog et users
./scripts/dev-all.sh catalog users

# Démarrer tous les 5 services
./scripts/dev-all.sh all

# Démarrer en arrière-plan (sans fenêtres de terminal)
./scripts/dev-all.sh --no-terminal catalog users

# Vérifier l'infrastructure sans démarrer les services
./scripts/dev-all.sh --check

# Lister les services disponibles
./scripts/dev-all.sh --list
```

**Terminaux supportés** : gnome-terminal, konsole, xfce4-terminal, xterm, kitty, alacritty

### Mode production (start-all.sh / stop-all.sh)

Compile et lance les services en tant que JARs Java.

```bash
# Démarrer tous les services
./scripts/start-all.sh

# Démarrer en mode développement (quarkusDev)
./scripts/start-all.sh -d

# Démarrer en parallèle
./scripts/start-all.sh --parallel

# Exclure un service
./scripts/start-all.sh --skip notifications

# Démarrer uniquement certains services
./scripts/start-all.sh --only catalog --only users

# Arrêter tous les services
./scripts/stop-all.sh

# Arrêter des services spécifiques
./scripts/stop-all.sh catalog users

# Forcer l'arrêt (SIGKILL)
./scripts/stop-all.sh -f

# Avec timeout personnalisé
./scripts/stop-all.sh --timeout 60
```

### Compilation (build.sh)

```bash
# Compiler tous les services
./scripts/build.sh

# Nettoyer avant de compiler
./scripts/build.sh -c

# Compiler avec tests
./scripts/build.sh -t

# Compiler des services spécifiques
./scripts/build.sh catalog users

# Compilation parallèle
./scripts/build.sh -p

# Compiler des images natives (GraalVM requis)
./scripts/build.sh --native catalog

# Construire les images Docker
./scripts/build.sh --docker
```

### Surveillance (health-check.sh / status.sh)

```bash
# Vérifier tous les services
./scripts/health-check.sh

# Mode surveillance continue (refresh toutes les 5s)
./scripts/health-check.sh -w

# Vérification détaillée
./scripts/health-check.sh -v catalog

# Vérifier uniquement l'infrastructure
./scripts/health-check.sh --infra

# Vérifier uniquement les microservices
./scripts/health-check.sh --services

# Sortie JSON
./scripts/health-check.sh --json

# Afficher le statut complet du système
./scripts/status.sh

# Statut court (services uniquement)
./scripts/status.sh -s
```

### Logs (logs.sh)

```bash
# Voir les logs d'un service
./scripts/logs.sh catalog

# Suivre les logs en temps réel
./scripts/logs.sh -f catalog

# Afficher les N dernières lignes
./scripts/logs.sh -n 50 catalog

# Voir tous les logs agrégés
./scripts/logs.sh --all

# Logs de conteneurs Docker
./scripts/logs.sh --docker kafka

# Filtrer par pattern
./scripts/logs.sh catalog --grep "ERROR"

# Afficher uniquement les erreurs
./scripts/logs.sh catalog --errors

# Logs depuis un certain temps
./scripts/logs.sh --docker kafka --since 1h
```

### Nettoyage (clean.sh)

```bash
# Nettoyer les artifacts de compilation
./scripts/clean.sh

# Nettoyer tout (build + logs + docker + gradle)
./scripts/clean.sh -a

# Nettoyer les fichiers de log
./scripts/clean.sh --logs

# Nettoyer les ressources Docker
./scripts/clean.sh --docker

# Nettoyer les caches Gradle
./scripts/clean.sh --gradle

# Sans confirmation
./scripts/clean.sh -f -a
```

### Vérification de l'environnement (env-check.sh)

```bash
# Vérifier que l'environnement est prêt
./scripts/env-check.sh
```

Ce script vérifie : Java 21+, Docker, Docker Compose, Gradle, et les ports disponibles.

## Configuration

### Profils Quarkus

- `%dev` : Développement local (Dev Services désactivés)
- `%test` : Tests automatisés
- `%prod` : Production

### Variables d'environnement

Le fichier `docker/.env` contient les variables de configuration :

```bash
# Base de données
APP_DB_USER=training
APP_DB_PASSWORD=training

# Grafana
GRAFANA_USER=admin
GRAFANA_PASSWORD=admin
```

### Configuration partagée

Le module `shared-config` contient la configuration commune :
- Connexion PostgreSQL
- Configuration Redis
- OpenTelemetry
- Micrometer/Prometheus
- Format des logs

## Dépannage

### Le service ne démarre pas

```bash
# Vérifier les ports
./scripts/health-check.sh --services

# Consulter les logs
./scripts/logs.sh catalog --errors
```

### L'infrastructure ne répond pas

```bash
# Vérifier Docker
docker ps

# Redémarrer l'infrastructure
./scripts/infra-down.sh && ./scripts/infra-up.sh
```

### Problèmes de mémoire

```bash
# Vérifier les ressources
./scripts/status.sh

# Nettoyer Docker
./scripts/clean.sh --docker
```

### Logs et traces

```bash
# Suivre les logs en temps réel
./scripts/logs.sh -f catalog

# Voir uniquement les erreurs
./scripts/logs.sh catalog --errors

# Logs Docker
./scripts/logs.sh --docker kafka
```

## API REST

### Catalog Service (8081)

| Méthode | Endpoint | Description |
|---------|----------|-------------|
| GET | `/api/resources` | Liste des ressources |
| GET | `/api/resources/{id}` | Détail d'une ressource |
| POST | `/api/resources` | Créer une ressource |
| PUT | `/api/resources/{id}` | Modifier une ressource |
| DELETE | `/api/resources/{id}` | Supprimer une ressource |

### Users Service (8082)

| Méthode | Endpoint | Description |
|---------|----------|-------------|
| GET | `/api/users` | Liste des utilisateurs |
| GET | `/api/users/{id}` | Détail d'un utilisateur |
| POST | `/api/users` | Créer un utilisateur |

### Reviews Service (8083)

| Méthode | Endpoint | Description |
|---------|----------|-------------|
| GET | `/api/reviews` | Liste des avis |
| GET | `/api/reviews/resource/{id}` | Avis d'une ressource |
| POST | `/api/reviews` | Créer un avis |

## Technologies utilisées

- **Quarkus 3.x** - Framework Java supersonic
- **Gradle 8.x** - Build multi-modules
- **PostgreSQL 16** - Base de données
- **Redis 7** - Cache distribué
- **Apache Kafka 3.9** - Messaging
- **Consul 1.19** - Service Discovery
- **OpenTelemetry** - Tracing distribué
- **Zipkin 3.4** - Backend de traces
- **Prometheus 2.54** - Collecte de métriques
- **Grafana 11.2** - Visualisation
- **SmallRye Stork** - Load balancing client-side

## Ressources

- [Documentation Quarkus](https://quarkus.io/guides/)
- [12-Factor App](https://12factor.net/fr/)
- [OpenTelemetry](https://opentelemetry.io/)
- [SmallRye Stork](https://smallrye.io/smallrye-stork/)

## Licence

Ce projet est fourni à des fins éducatives et de formation.
