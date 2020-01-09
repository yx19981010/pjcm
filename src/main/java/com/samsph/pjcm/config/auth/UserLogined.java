package com.samsph.pjcm.config.auth;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserLogined {
    int userId;
    int userRole;
    String userEmail;
}
