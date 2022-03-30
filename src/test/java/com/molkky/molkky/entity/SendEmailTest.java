package com.molkky.molkky.entity;


import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.service.EmailSenderService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.event.EventListener;

@SpringBootTest(classes = MolkkyApplication.class)
public class SendEmailTest {
    @Autowired
    private EmailSenderService senderService;

    @Test
    @EventListener(ApplicationReadyEvent.class)
    public void sendMail(){
        senderService.SendEmail("pierre.menanteau@reseau.eseo.fr","test","Un mail envoyé en java !");
    }

}
