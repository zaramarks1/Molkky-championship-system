package com.molkky.molkky.controllers;

import com.molkky.molkky.service.EmailSenderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

@Controller
public class EmailController {

    @Autowired
    EmailSenderService emailSenderService;

    @GetMapping("/emailButton")
    public String createUser(Model model) {
        return "/emailButton";
    }

    @PostMapping("/sendMail")
    public String saveUser() {
        emailSenderService.sendEmail("sacha.thuault@gmail.com", "Test", "Fonctionnement de l'envoi d'email");
        return "redirect:/emailButton";
    }
}
