package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

@SpringBootTest(classes = MolkkyApplication.class)
class EmailSenderServiceTest {
    @Autowired
    private EmailSenderService emailSenderService;

    static String getCurrentGitBranch() throws IOException, InterruptedException {
        Process process = Runtime.getRuntime().exec( "git rev-parse --abbrev-ref HEAD" );
        process.waitFor();

        BufferedReader reader = new BufferedReader(
                new InputStreamReader( process.getInputStream() ) );

        return reader.readLine();
    }

    @Test
    void testMail() throws IOException, InterruptedException {
        if(getCurrentGitBranch().equals("US53DEV_Sacha")){
            emailSenderService.sendEmail("calembel.molkky@gmail.com", "GPI LESGO", "Fonctionnement de l'envoi de test");
        }
    }
}
