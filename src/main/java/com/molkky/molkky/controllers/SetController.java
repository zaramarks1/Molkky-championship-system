package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.SetService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

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
    public String updateSet(Model model, @RequestBody SetModel setModel, HttpSession session) {
        User user = (User)session.getAttribute("user");
        if(Boolean.FALSE.equals(setService.isUserInSet(setModel, UserService.createUserModel(user)))) {
            return "not in match";
        }
//        matchRepository.save(match);
        Match match = (Match)model.getAttribute("match");
        return "redirection: /matches/match?match_id=" + match.getId();
    }

}
