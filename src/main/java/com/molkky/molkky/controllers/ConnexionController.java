package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserConnectionModel;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.ConnexionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;
import type.UserRole;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

@Controller
public class ConnexionController {
    @Autowired
    private ConnexionService connexionService;

    @Autowired
    private UserRepository userRepository;

    private final String changePageConnection = "redirect:/connexion";

    @GetMapping("/connexion")
    public String Home(Model  model,HttpSession session){
        session.invalidate();
        User user = new User();
        model.addAttribute("user" ,user);

        UserConnectionModel userConnectionModel = new UserConnectionModel();

        model.addAttribute("userConnection", userConnectionModel);
        return "connexion";
    }

    @PostMapping("/connexion")
    public ModelAndView connexionUser(@ModelAttribute("userConnection")UserConnectionModel userModel, HttpServletRequest request){

        try {
            if(userRepository.existsUserByEmailAndPassword(userModel.getEmail(), userModel.getPassword())){
                User user = userRepository.findUserByEmailAndPassword(userModel.getEmail(), userModel.getPassword());
                if(userModel.getCode() != null){
                    //TODO add player condition
                }
            }else{
                return new ModelAndView(changePageConnection);
            }
            return new ModelAndView("redirect:/");

        }catch  (Exception e){
            e.printStackTrace();
        }
        return new ModelAndView(changePageConnection);
    }

    @PostMapping("/connexion2")
    public ModelAndView connexionUser2(@ModelAttribute("userConnection")User user, HttpServletRequest request){
        try {
            User userByEmail = userRepository.findUserByEmail(user.getEmail());
            userByEmail.setPseudo("test2");
            userRepository.save(userByEmail);
            if (connexionService.decode(user.getPassword(), userByEmail)) {
                //UserRole role = userByEmail.getRole();
                //request.getSession().setAttribute("role",role);
                request.getSession().setAttribute("user",userByEmail);
                return new ModelAndView("redirect:/");
            } else {
                return new ModelAndView(changePageConnection);
            }
        }catch  (Exception e){
            e.printStackTrace();
        }
        return new ModelAndView(changePageConnection);
    }
}
