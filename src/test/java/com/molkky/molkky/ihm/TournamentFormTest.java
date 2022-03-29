package com.molkky.molkky.ihm;

import com.molkky.molkky.SeleniumConfig;
import org.junit.jupiter.api.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TournamentFormTest {
    private SeleniumConfig config;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
    }

    @Test
    void testTournamentForm() {
        config.getDriver().get("https://google.fr");
        Assertions.assertEquals("Google", config.getDriver().getTitle());
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
