package com.molkky.molkky.service;

import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.UserModel;
import org.springframework.stereotype.Service;

@Service
public class MatchService {
    public Integer getUserTeamIndex(MatchModel match, UserModel user) {
        if (match.getTeam1().getUser().getId().equals(user.getId())) {
            return 0;
        } else if (match.getTeam2().getUser().getId().equals(user.getId())) {
            return 1;
        } else {
            return -1;
        }
    }
}
