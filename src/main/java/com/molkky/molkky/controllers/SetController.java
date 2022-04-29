package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.SetRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;

@Controller
public class SetController {
    @Autowired
    private SetRepository setRepository;

    @PostMapping("/set/updateSet")
    public ModelAndView updateSet(Model model,
                                  HttpSession session,
                                  @RequestParam(name = "set_id", required = true) Integer id,
                                  @RequestParam(name = "score", required = true) Integer score,
                                  @RequestParam(name = "team_index", required = true) Integer teamIndex) {
        User user = (User)session.getAttribute("user");
        Set set = setRepository.findById(id);
//        if(!Objects.equals(user.getTeam().getId(), set.getTeams().get(teamIndex).getId())) {
//            return "Pas dans la bonne équipe";
//        }
        if(teamIndex == 0) {
            set.setScoreTeam1(score);
        }
        else {
            set.setScoreTeam2(score);
        }
        setRepository.save(set);
        model.addAttribute("match_id", set.getMatch().getId());
        return new ModelAndView("redirect:/matches/match?match_id=" + set.getMatch().getId());
    }

}
