package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.service.ConnexionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.Banner;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;
import org.w3c.dom.stylesheets.LinkStyle;

@Controller
public class ConnexionController {
    @Autowired
    private ConnexionService connexionService;


    @GetMapping("/connexion")
    public String Home(Model  model){
        User user = new User();
        model.addAttribute("user" ,user);
        return "/connexion";
    }

    @PostMapping("/connexion")
    public ModelAndView connexionUser(@ModelAttribute("user")User user){
        return new ModelAndView("redirect:/connexion");
    }

}
