package com.akfc.hello;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "greeting")
public interface HelloConfig {
    String message();
    String name();
}
