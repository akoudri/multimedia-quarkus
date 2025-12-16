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
| **PgAdmin** | - | Via Traefik |

### Gestion de l'infrastructure

```bash
# Démarrer
./scripts/infra-up.sh

# Arrêter
./scripts/infra-down.sh

# Arrêter et supprimer les volumes (⚠️ perte de données)
./scripts/infra-down.sh -v
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

Tous les scripts suivent les bonnes pratiques de la méthodologie **12-Factor App**.

| Script | Description |
|--------|-------------|
| `env-check.sh` | Vérifier l'environnement de développement |
| `infra-up.sh` | Démarrer l'infrastructure Docker |
| `infra-down.sh` | Arrêter l'infrastructure |
| `dev.sh` | Lancer un service en mode développement |
| `start-all.sh` | Démarrer tous les microservices |
| `stop-all.sh` | Arrêter gracieusement les services |
| `health-check.sh` | Vérifier l'état de santé |
| `status.sh` | Afficher le statut complet |
| `logs.sh` | Consulter les logs |
| `build.sh` | Compiler les services |
| `clean.sh` | Nettoyer les artifacts |

Documentation complète : [scripts/README.md](scripts/README.md)

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
