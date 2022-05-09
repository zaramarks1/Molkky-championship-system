package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.UserControllerExt;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;

@Controller
public class HomeController extends UserControllerExt {

    @GetMapping("/")
    public String Index(Model model, HttpSession session){
//        UserLogged user = (UserLogged)session.getAttribute("user");
//        model.addAttribute("user", user);
        return "/home";
    }

}
