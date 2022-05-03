package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;

@Controller
public class SetController {
    @Autowired
    private SetRepository setRepository;

    @Autowired
    private MatchService matchService;

    @Autowired
    private UserService userService;

    @Autowired
    private SetService setService;

    @PostMapping("/set/updateSet")
    public ModelAndView updateSet(Model model,
                                  HttpSession session,
                                  @RequestBody SetModel setModel) {
        UserModel user = UserService.createUserModel((User)session.getAttribute("user"));
//        if(!Objects.equals(user.getTeam().getId(), set.getTeams().get(teamIndex).getId())) {
//            return "Pas dans la bonne équipe";
//        }

        setService.enterSetResults(setModel, user);
        Integer matchId = setService.getSetFromModel(setModel).getMatch().getId();

        model.addAttribute("match_id", matchId);
        return new ModelAndView("redirect:/matches/match?match_id=" + matchId.toString());
    }

}
