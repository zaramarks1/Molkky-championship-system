package com.molkky.molkky.controllers;

import com.molkky.molkky.service.EmailSenderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.List;

@Controller
public class SessionControllerExample {
    @Autowired
    private EmailSenderService emailSenderService;

    String messagesAttribute = "MY_SESSION_MESSAGES";

    String redirect = "redirect:/session";

    @GetMapping("/session")
    public String home(Model model, HttpSession session) {
        @SuppressWarnings("unchecked")
        List<String> messages = (List<String>) session.getAttribute(messagesAttribute);

        if (messages == null) {
            messages = new ArrayList<>();
        }
        model.addAttribute("sessionMessages", messages);
        model.addAttribute("sessionId", session.getId());

        return "/session";
    }

    @PostMapping("/persistMessage")
    public String persistMessage(@RequestParam("msg") String msg, HttpServletRequest request) {
        @SuppressWarnings("unchecked")
        List<String> msgs = (List<String>) request.getSession().getAttribute(messagesAttribute);
        if (msgs == null) {
            msgs = new ArrayList<>();
            request.getSession().setAttribute(messagesAttribute, msgs);
        }
        msgs.add(msg);
        request.getSession().setAttribute(messagesAttribute, msgs);
        request.getSession().setAttribute("SESSION_TEST","yeah");
        return redirect;
    }

    @PostMapping("/destroy")
    public String destroySession(HttpServletRequest request) {
        request.getSession().invalidate();
        return redirect;
    }
    @PostMapping("/sendMail")
    public String sendMail() {
        emailSenderService.sendEmail("sacha.thuault@gmail.com", "GPI LESGO", "Yes");
        return redirect;
    }
}
