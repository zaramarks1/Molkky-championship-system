package com.molkky.molkky.service;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.TournamentStatus;
import type.UserRole;

import java.util.Date;
import java.util.List;

import static com.molkky.molkky.utility.StringUtilities.createCode;


@Service
public class TournamentService {

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;


    public Tournament create(TournamentModel tournamentModel) {
        Tournament tournament = new Tournament(tournamentModel);

        tournament = tournamentRepository.save(tournament);

        String mail = tournamentModel.getEmail();

        User user = new User();

        if (!userRepository.existsUserByEmail(mail)) {
            user.setEmail(mail);
            user.setPassword(createCode(5));
            user = userRepository.save(user);
        } else {
            user = userRepository.findUserByEmail(mail);
        }

        UserTournamentRole userTournamentRole = new UserTournamentRole();

        userTournamentRole.setUser(user);
        userTournamentRole.setRole(UserRole.ADM);
        userTournamentRole.setTournament(tournament);

        userTournamentRoleRepository.save(userTournamentRole);

        return tournament;
    }

    // Fonction qui test si le nombre d'équipes inscrites après la date limite d'inscription est suffisant. Sinon, delete le tournament
    public boolean isMinimumTeamsBeforeDate() {
        Date currentDate = new Date();
        List<Tournament> tournaments = tournamentRepository.findAll();
        boolean condition = true;

        for (Tournament tournament : tournaments) {
            if (currentDate.after(tournament.getCutOffDate())) {
                condition = tournament.getTeams().size() >= tournament.getMinTeam();
                if (!condition) {
                    tournamentRepository.delete(tournament);
                    System.out.println(tournament.getName() + "DELETED");
                }
            }
        }
        return condition;
    }

    public void registerClosedForTournament() {
        Date currentDate = new Date();
        List<Tournament> tournaments = tournamentRepository.findAll();
        boolean condition = true;

        for (Tournament tournament : tournaments) {
            {
                if (tournament.getStatus() == TournamentStatus.AVAILABLE) {
                    if (new Date().after(tournament.getCutOffDate()) || tournament.getMaxTeam() == tournament.getTeams().size()) {
                        tournament.setVisible(false);
                        tournamentRepository.save(tournament);

                        System.out.println(tournament.getName() + "CLOSED");
                    }
                }
            }
        }
    }
}
