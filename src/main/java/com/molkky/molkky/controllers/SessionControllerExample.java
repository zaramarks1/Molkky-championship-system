package com.molkky.molkky.controllers;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class SessionControllerExample {

    @GetMapping("/session")
    public String home(Model model, HttpSession session) {
        @SuppressWarnings("unchecked")
        List<String> messages = (List<String>) session.getAttribute("MY_SESSION_MESSAGES");

        if (messages == null) {
            messages = new ArrayList<>();
        }
        model.addAttribute("sessionMessages", messages);
        model.addAttribute("sessionId", session.getId());

        return "session";
    }

    @PostMapping("/persistMessage")
    public String persistMessage(@RequestParam("msg") String msg, HttpServletRequest request) {
        @SuppressWarnings("unchecked")
        List<String> msgs = (List<String>) request.getSession().getAttribute("MY_SESSION_MESSAGES");
        if (msgs == null) {
            msgs = new ArrayList<>();
            request.getSession().setAttribute("MY_SESSION_MESSAGES", msgs);
        }
        msgs.add(msg);
        request.getSession().setAttribute("MY_SESSION_MESSAGES", msgs);
        request.getSession().setAttribute("SESSION_TEST","yeah");
        return "redirect:/session";
    }

    @PostMapping("/destroy")
    public String destroySession(HttpServletRequest request) {
        request.getSession().invalidate();
        return "redirect:/session";
    }
    @PostMapping("/goToRegister")
    public String goToRegister() {
        return "redirect:/register";
    }
}
