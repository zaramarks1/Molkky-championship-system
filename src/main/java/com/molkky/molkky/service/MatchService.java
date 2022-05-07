package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.UserTounamentRole;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.MatchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.SetTeamIndex;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
public class MatchService {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private UserService userService;
    public SetTeamIndex getUserTeamIndex(MatchModel match, UserModel user) {
        Match matchEntity = getMatchFromModel(match);

        for(UserTounamentRole u : matchEntity.getTeams().get(0).getUserTounamentRoles()) {
            if(Objects.equals(u.getUser().getId(), user.getId())) {
                return SetTeamIndex.TEAM1;
            }
        }
        for(UserTounamentRole u : matchEntity.getTeams().get(1).getUserTounamentRoles()) {
            if(Objects.equals(u.getUser().getId(), user.getId())) {
                return SetTeamIndex.TEAM2;
            }
        }
        return SetTeamIndex.ORGA;
    }

    public static MatchModel getMatchModelFromEntity(Match match) {
        MatchModel matchModel = new MatchModel();
        matchModel.setId(match.getId());
        matchModel.setFinished(match.getFinished());
        matchModel.setNbSets(match.getNbSets());
        return matchModel;
    }

    public Boolean isUserInMatch(MatchModel match, UserModel user) {
        Match matchEntity = getMatchFromModel(match);
        for(UserTounamentRole u : matchEntity.getTeams().get(0).getUserTounamentRoles()) {
            if(Objects.equals(u.getUser().getId(), user.getId())) {
                return true;
            }
        }
        for(UserTounamentRole u : matchEntity.getTeams().get(1).getUserTounamentRoles()) {
            if(Objects.equals(u.getUser().getId(), user.getId())) {
                return true;
            }
        }
        return false;
    }

    public static List<MatchModel> createMatchModels(List<Match> matches) {
        List<MatchModel> matchModels = new ArrayList<>();
        if (matches != null) {
            for (Match match : matches) {
                if(match.getId() != null) {
                    matchModels.add(getMatchModelFromEntity(match));
                }
            }
        }
        return matchModels;
    }

    private Match getMatchFromModel(MatchModel matchModel) {
        return matchRepository.findById(matchModel.getId());

    }
}
