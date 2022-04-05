package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.RegisterService;
import org.apache.commons.lang3.builder.ToStringExclude;
import org.junit.Assert;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class LoginTest {

    @Autowired
    private RegisterService registerService;
    @Autowired
    private TournamentRepository tournamentRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp(){
        config =new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @Test
    void testLoginGetPage(){
        config.getDriver().get(url+"/connexion");
        Assertions.assertEquals("Page de connexion", config.getDriver().getTitle());
    }


    @Test
    void testConnexionDisplayed(){
        config.getDriver().get(url + "/connexion");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("mail")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("code")).isDisplayed());
    }

    @Test
    void testConnexionUserGood(){
        config.getDriver().get(url + "/connexion");
        String user="pierremenanteau@hotmail.fr";
        String code="1,$.76?.15";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/tournament/create", config.getDriver().getCurrentUrl());
    }
    @Test
    void testConnexionUserFalse() {
        config.getDriver().get(url + "/connexion");
        String user="failTets@hotmail.fr";
        String code="fail";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("code")).sendKeys(code);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
    }

    @Test
    void testCreateUser(){
        config.getDriver().get(url + "/register");
        String user = "userTest@gmail.com";
        config.getDriver().findElement(new By.ById("mail")).sendKeys(user);
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals(url + "/connexion", config.getDriver().getCurrentUrl());
        String code = "";

    }
    @AfterAll
    void tearDown(){
        config.getDriver().quit();
    }

}
