package com.molkky.molkky.ihm;

import com.molkky.molkky.SeleniumConfig;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

public class TournamentFormTest {
    private SeleniumConfig config;

    @Before
    public void setUp() {
        config = new SeleniumConfig();
    }

    @Test
    public void createTournamentForm() {
        config.getDriver().get("https://google.fr");
        Assertions.assertEquals("Google", config.getDriver().getTitle());
    }

    @After
    public void tearDown() {
        config.getDriver().quit();
    }
}
