package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.AddCourt;
import com.molkky.molkky.model.AddCourtList;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import java.util.List;

@Controller
@RequestMapping("/court")
public class CourtController {
    @Autowired
    UserRepository userRepository;
    @Autowired
    CourtRepository courtRepository;
    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @PostMapping("/add")
    public String addCourt(@ModelAttribute("courts") AddCourtList courts){

        Tournament tournament = tournamentRepository.findById(courts.getCourts().get(0).getTournamentId());
        List<Court> courtList = tournament.getCourts();
        for(AddCourt court: courts.getCourts()){
            Court newCourt = new Court();
            newCourt.setName(court.getName());
            newCourt.setTournament(tournament);
            newCourt.setAvailable(true);
            courtRepository.save(newCourt);
            courtList.add(newCourt);
        }
        tournament.setCourts(courtList);
        tournamentRepository.save(tournament);

        return "redirect:/tournament/view?tournamentId=" + tournament.getId();

    }
}
