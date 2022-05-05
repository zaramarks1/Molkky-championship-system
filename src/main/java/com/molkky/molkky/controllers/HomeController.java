package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import javax.servlet.http.HttpSession;

@Controller
public class HomeController {

    @GetMapping("/")
    public String Index(Model model, HttpSession session){
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);
        return "/home";
    }

}
