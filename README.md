# Haskell

Репозиторий с заданиями по курсу функционального программирования.

## Установка Nix

```bash
# Install Nix with flake support
curl -L https://nixos.org/nix/install | sh

# Restart your shell or source the nix configuration
. ~/.nix-profile/etc/profile.d/nix.sh
```

## Настройка окружения

```bash
nix develop
```

## Запуск тестов

```bash
stack test
```

## Запуск интерпретатора с модулями репозитория

```bash
stack ghci --test
```
