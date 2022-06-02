package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;


@Controller
public class InfosController extends DefaultAttributes {

    String redirect = "redirect:/infos";

    @Autowired
    UserRepository userRepository;
    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        return "infos";
    }

    @PostMapping("/changePseudo")
    public String modifyPseudo(Model model,HttpSession session,@RequestParam(name="pseudo") String pseudo ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setPseudo(pseudo);
        userRepository.save(userDB);
        user.setPseudo(pseudo);
        session.setAttribute("user",user);
        return redirect;
    }
    @PostMapping("/changeSurname")
    public String modifySurname(Model model,HttpSession session,@RequestParam(name="surname") String surname ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setSurname(surname);
        userRepository.save(userDB);
        user.setSurname(surname);
        session.setAttribute("user",user);
        return redirect;
    }

    @PostMapping("/changeForename")
    public String modifyForename(Model model,HttpSession session,@RequestParam(name="forename") String forename ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setForename(forename);
        userRepository.save(userDB);
        user.setForename(forename);
        session.setAttribute("user",user);
        return redirect;
    }

    @PostMapping("/changePassword")
    public String modifyPassword(Model model,HttpSession session,@RequestParam(name="pwd1") String pwd1, @RequestParam(name="pwd2") String pwd2 ){
        if(pwd1.equals(pwd2)) {
            UserLogged user = getUser(session);
            User userDB = userRepository.findById(user.getId());
            userDB.setPassword(pwd1);
            userRepository.save(userDB);
            user.setPassword(pwd2);
            session.setAttribute("user", user);
        }
        return redirect;
    }


}
