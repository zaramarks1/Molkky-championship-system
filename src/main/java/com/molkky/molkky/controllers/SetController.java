package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import type.SetTeamIndex;

import javax.servlet.http.HttpSession;

@Controller
public class SetController extends DefaultAttributes {
    @Autowired
    private SetRepository setRepository;

    @Autowired
    private MatchService matchService;

    @Autowired
    private UserService userService;

    @Autowired
    private SetService setService;

    @PostMapping("/sets/updateSet")
    public String updateSet(Model model, @ModelAttribute SetModel setModel, HttpSession session) {
        UserLogged user = (UserLogged)model.getAttribute("user");
        if(user == null) return "redirect:/connexion";
        Set set = setService.getSetFromModel(setModel);
        SetTeamIndex setTeamIndex = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(set.getMatch()), UserService.createUserModelFromUserLogged(user));
        if(setTeamIndex == null) return null;
        switch (setTeamIndex) {
            case TEAM1:
                set.setScore1Team1(setModel.getScore1Team1());
                set.setScore2Team1(setModel.getScore2Team1());
                break;
            case TEAM2:
                set.setScore1Team2(setModel.getScore1Team2());
                set.setScore2Team2(setModel.getScore2Team2());
                break;
            case ORGA:
                set.setScore1Orga(setModel.getScore1Orga());
                set.setScore2Orga(setModel.getScore2Orga());
                break;
            default:
                break;
        }
        setRepository.save(set);
        return "redirect:/matches/match?match_id=" + set.getMatch().getId();
    }

}
