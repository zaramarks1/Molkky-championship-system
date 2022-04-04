package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class)
public class EmailSenderServiceTest {
    @Autowired
    private EmailSenderService emailSenderService;

    @Test
    void testMail(){
        emailSenderService.SendEmail("pierre.menanteau@reseau.eseo.fr", "GPI LESGO", "Fonctionnement de l'envoie de test");
    }
}
