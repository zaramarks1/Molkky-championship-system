package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TournamentFormTest {
    private SeleniumConfig config;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
    }

    @Test
    void testTournamentForm() {
        config.getDriver().get("http://localhost:8080/tournament/create");
        Assertions.assertEquals("Creation Nouveau Tournoi", config.getDriver().getTitle());
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
