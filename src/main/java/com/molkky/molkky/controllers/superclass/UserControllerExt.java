package com.molkky.molkky.controllers.superclass;

import com.molkky.molkky.model.UserLogged;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpSession;

@Controller
public class UserControllerExt {
    @ModelAttribute("user")
    public UserLogged getUser(HttpSession session) {
        return session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
    }
}
