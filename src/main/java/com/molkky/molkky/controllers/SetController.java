package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.SetService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;

import javax.servlet.http.HttpSession;

@Controller
public class SetController extends DefaultAttributes {
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private SetService setService;

    @PostMapping("/sets/updateSet")
    public String updateSet(Model model, @ModelAttribute SetModel setModel, HttpSession session) {
        UserLogged user = (UserLogged)model.getAttribute("user");
        if(user == null) return "redirect:/connexion";
        Set set = setService.getSetFromModel(setModel);
        setService.enterSetResults(setModel, new UserTournamentRoleModel(userTournamentRoleRepository.findById(user.getTournamentRoleId())));
        return "redirect:/matches/match?match_id=" + set.getMatch().getId();
    }

}
